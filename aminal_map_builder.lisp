(defpackage :aminals
  (:use :cl)
  (:export
   #:defbyteconverter))

(in-package :aminals)
;;; utils
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro formatln (&optional (fmt "") &rest args)
  `(format t ,(concatenate 'string fmt "~%") ,@args))

(defun num-params (prefix num)
  (let ((result))
    (dotimes (x (eval num))
      (push x result))
    (mapcar
        (lambda (x) (read-from-string (format nil "~a~a" prefix x)))
        (reverse result))))

(defmacro plist-put (plist key value)
  `(setf (getf ,plist ,key) ,value))

;;; byte converters
(defmacro defbyteconverter (name num-bytes &body body)
    (let ((x (num-params 'b num-bytes)))
      `(defun ,name ,x ,@body)))

(defbyteconverter u32-from-bytes 4
  (logior
   (ash b0 0)
   (ash b1 8)
   (ash b2 16)
   (ash b3 24)))

(defbyteconverter u16-from-bytes 2
  (logior
   (ash b0 0)
   (ash b1 8)))

(defbyteconverter guid-from-bytes 16
  (list (u32-from-bytes b0 b1 b2 b3)
        (u16-from-bytes b4 b5)
        (u16-from-bytes b6 b7)
        (list b8 b9 b10 b11 b12 b13 b14 b15)))

(defbyteconverter s24-from-bytes 3
  (let ((val (logior (ash b0 0) (ash b1 8) (ash b2 16))))
    (if (logbitp 23 val)
        (- val #x1000000)
        val)))

(defun s24-to-float (n)
  (/ (float n) 8388608.0))  ; 2^23

(defbyteconverter stereo-frame-s24-from-bytes 6
  (list :L (s24-from-bytes b0 b1 b2)
        :R (s24-from-bytes b3 b4 b5)))

(defun char-code-string-from-bytes (&rest elements)
  (let ((result ""))
    (for-list x elements
      (setf result (concatenate 'string result (char-to-string (code-char x)))))
    result))

(defmacro funcall-with-contiguous-bytes (fn seq num-bytes)
  (let ((x 
          (let ((result nil))
            (dotimes (x num-bytes)
              (push `(elt ,seq ,x) result))
            (reverse result))))
    `(funcall ,fn ,@x)))

;; bytereader pattern
(defmacro defbytereader (name fn count)
  `(defmacro ,name ((stream &optional (var (gensym))) &body body )
     (with-gensyms (x)
       `(let ((,x (make-array ,',count :element-type '(unsigned-byte 8))))
          (read-sequence ,x ,stream)
          (let ((,var (funcall-with-contiguous-bytes ,',fn ,x ,',count)))
            ,var
            ,@body)))))

(defbytereader with-read-u32
  #'u32-from-bytes 4)

(defbytereader with-read-riff-tag
  #'char-code-string-from-bytes 4)

(defbytereader with-read-u16
  #'u16-from-bytes 2)

(defbytereader with-read-guid
  #'guid-from-bytes 16)

(defbytereader with-stereo-frame-s24
  #'stereo-frame-s24-from-bytes 6)

(defmacro for-list (var list &body body)
  (let ((l (gensym)))
    `(do* ((,l ,list (cdr ,l)) (,var (car ,l) (car ,l))) ((eql ,l nil))
       ,@body)))

(defparameter *WAVEFORMATEX* 65534)

(defun load-wav-file (file-name)
  (with-open-file (f file-name :if-does-not-exist nil :element-type '(unsigned-byte 8))
    (if f
        (formatln "~a Opened" file-name)
        (formatln "~a Not Opened" file-name))
    (when f
      (let ((metadata nil))
        (with-read-riff-tag (f)
          (with-read-u32 (f file-size)
            (plist-put metadata :size-bytes file-size)))
        (with-read-riff-tag (f wave))
        (with-read-riff-tag (f fmt)
          (with-read-u32 (f format-chunk-size))
          (with-read-u16 (f type)
          ;; WAVEFORMATEX do we need to worry about other types?
            (when (eql type *WAVEFORMATEX*)
              (with-read-u16 (f num-chans) (plist-put metadata :num-chans num-chans))
              (with-read-u32 (f sample-rate) (plist-put metadata :sample-rate sample-rate))
              (with-read-u32 (f bytes-per-second))
              (with-read-u16 (f block-align))
              (with-read-u16 (f bits-per-sample) (plist-put metadata :bits-per-sample bits-per-sample))
              (with-read-u16 (f cb-size))
              (with-read-u16 (f samples))
              (with-read-u32 (f dw-channel-mask))
              (with-read-guid (f guid))
              (with-read-riff-tag (f fact)
                (with-read-u32 (f fact-chunk-size))
                (with-read-u32 (f num-samples) (plist-put metadata :num-samples num-samples))))))
        (with-read-riff-tag (f data))
        (with-read-u32 (f data-size))
        (dotimes (x (getf metadata :num-samples))
          (with-stereo-frame-s24 (f frame)
            (formatln "L: ~f R: ~f"
                      (s24-to-float (getf frame :L))
                      (s24-to-float (getf frame :R)))))
        (formatln "Metadata: ~a" metadata)))))

(load-wav-file "Flocks_G6.wav")
