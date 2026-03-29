(defpackage :aminals
  (:use :cl)
  (:export
   #:defbyteconverter))

(in-package :aminals)
;;; utils
(defmacro with-gensyms (syms &body body)
  "From On Lisp, this creates symbols from a list using (gensym) to be used in macros"
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

(defmacro for-list (var list &body body)
  (with-gensyms (l)
    `(do* ((,l ,list (cdr ,l))
           (,var (car ,l) (car ,l)))
          ((eql ,l nil))
       ,@body)))

(defmacro for-array (var array &body body)
  (with-gensyms (x)
    `(do ((,x 0 (+ ,x 1)))
       ((= ,x (length ,array)))
     (let ((,var (aref ,array ,x)))
       ,@body))))

;; (for-array x #(1 2 3) (print x))

(defmacro array-put (array i v)
  `(setf (aref ,array ,i) ,v))

(defmacro formatln (&optional (fmt "") &rest args)
  `(format t ,(concatenate 'string fmt "~%") ,@args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun num-params (prefix num)
    (let ((result))
      (dotimes (x (eval num))
        (push x result))
      (mapcar
       (lambda (x) (read-from-string (format nil "~a~a" prefix x)))
       (reverse result)))))

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

(defun char-to-string (c)
  "Takes a char and returns it as a string"
  (format nil "~a" c))

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



(defparameter *WAVEFORMATEX* 65534)

(defun load-wav-file (file-name &key (print-samples nil))
  "Loads a wav file and returns the metadata and the samples as values"
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
              (with-read-u16 (f num-chans)
                (plist-put metadata :num-chans num-chans))
              (with-read-u32 (f sample-rate)
                (plist-put metadata :sample-rate sample-rate))
              (with-read-u32 (f bytes-per-second))
              (with-read-u16 (f block-align))
              (with-read-u16 (f bits-per-sample)
                (plist-put metadata :bits-per-sample bits-per-sample))
              (with-read-u16 (f cb-size))
              (with-read-u16 (f samples))
              (with-read-u32 (f dw-channel-mask))
              (with-read-guid (f guid))
              (with-read-riff-tag (f fact)
                (with-read-u32 (f fact-chunk-size))
                (with-read-u32 (f num-samples)
                  (plist-put metadata :num-samples num-samples))))))
        (with-read-riff-tag (f data))
        (with-read-u32 (f data-size))
        (let* ((num-samples (getf metadata :num-samples))
               (samples (make-array (* num-samples 2)
                                    :element-type 'single-float))
               (left-i 0)
               (right-i 1))
          (dotimes (x num-samples)
            (with-stereo-frame-s24 (f frame)
              (let ((l (s24-to-float (getf frame :L)))
                    (r (s24-to-float (getf frame :R))))
                (when print-samples
                  (formatln "L: ~f R: ~f" l r))
                (array-put samples left-i l)
                (array-put samples right-i r)))
            (incf left-i 2)
            (incf right-i 2))
          (formatln "Metadata: ~a" metadata)
          (values metadata samples))))))

(defun write-f32-le (stream value)
  "Write a single-float as 4 little-endian bytes."
  (let ((bits (sb-kernel:single-float-bits value)))
    (write-byte (ldb (byte 8 0)  bits) stream)
    (write-byte (ldb (byte 8 8)  bits) stream)
    (write-byte (ldb (byte 8 16) bits) stream)
    (write-byte (ldb (byte 8 24) bits) stream)))

(multiple-value-bind (metadata samples) (load-wav-file "Samples/Flocks_A#0.wav")
  (print metadata)
  (when samples
    (with-open-file (f "test.bin"
                       :if-exists :supersede
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (for-array s samples
        (write-f32-le f s)))))

;; We can test these with:
;; ffmpeg -f f32le -ar 96000 -ac 2 -i test.bin -f pulse default

