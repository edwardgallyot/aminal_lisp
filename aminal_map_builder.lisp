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

(defun make-empty-string ()
  (make-array '(0)
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))

(defmacro for-array (var array &body body)
  (with-gensyms (x)
    `(do ((,x 0 (+ ,x 1)))
         ((= ,x (length ,array)))
       (let ((,var (aref ,array ,x)))
         ,@body))))

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

(defmacro list-push (list value)
  `(setf ,list (push ,value ,list)))

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
               (read-position (file-position f))
               (i 0))
          (file-position f read-position)
          (dotimes (x num-samples)
            (with-stereo-frame-s24 (f frame)
              (let ((l (s24-to-float (getf frame :L))))
                (when print-samples
                  (formatln "L: ~f" l))
                (array-put samples i l)))
            (incf i))
          (file-position f read-position)
          (dotimes (x num-samples)
            (with-stereo-frame-s24 (f frame)
              (let ((r (s24-to-float (getf frame :R))))
                (when print-samples
                  (formatln "R: ~f" r))
                (array-put samples i r)))
            (incf i))
          (values metadata samples))))))

(defun write-f32-le (stream value)
  "Write a single-float as 4 little-endian bytes."
  (let ((bits (sb-kernel:single-float-bits value)))
    (write-byte (ldb (byte 8 0)  bits) stream)
    (write-byte (ldb (byte 8 8)  bits) stream)
    (write-byte (ldb (byte 8 16) bits) stream)
    (write-byte (ldb (byte 8 24) bits) stream)))

(defun append-wav-file-f32s-to-stream (stream file-name)
  "Appends a load of wav file f32s to the stream and returns the metadata"
  (multiple-value-bind (metadata samples) (load-wav-file file-name)
    (when samples
      (for-array s samples
        (write-f32-le stream s)))
    metadata))

(defun c-tree (&rest rest)
  (apply #'list 'c-syntax rest))

(defun c-enum (id &key members)
  (list 'c-enum :id id :members members))

(defun c-id (id &key (value nil))
  (list 'c-id :id id :value value))

(defun c-var (type id &key (value nil))
  (list 'c-var :type type :id id :value value))

(defun c-value (value)
  (list 'c-var :value value))

(defun c-array (id &key (mods nil) type size (values nil))
  (list 'c-array :id id :mods mods :type type :size size :values values))

(defun c-record (id &key (members nil))
  (list 'c-record :id id :members members))

(defun c-fn (id &key (ret nil) (args nil) (body nil))
  (list 'c-fn :id id :ret ret :args args :body body))

(defmacro c-tree-append (tree code)
  `(setf ,tree (append ,tree (list ,code))))

(defmacro add-sample-to-gen-tree (path stream id members offsets sizes)
  (with-gensyms (metadata)
    `(let ((,metadata (append-wav-file-f32s-to-stream ,stream ,path)))
       (list-push ,members ,id)
       (list-push ,offsets (apply #'+ ,sizes))
       (list-push ,sizes (getf ,metadata :size-bytes)))))

(defun sample-spec (path id)
  (list :path path :id id))

(defmacro add-samples-to-gen-tree (specifiers stream members offsets sizes)
  (with-gensyms (var path id)
    `(for-list ,var ,specifiers
       (let ((,path (getf ,var :path))
             (,id (getf ,var :id)))
         (add-sample-to-gen-tree ,path ,stream ,id ,members ,offsets ,sizes)))))

(defun emit-c-enum (enum output-string)
  (let ((body (cdr enum)))
    (format output-string "enum ~a {~%" (getf body :id))
    (let ((members (getf body :members))
          (i 0))
      (for-list y members
          (let ((m (getf (cdr y) :id)))
            (format output-string "    ~a" (eval m))
            (when (< i (- (length members) 1))
              (format output-string ","))
            (format output-string "~%"))
        (incf i)))
    (format output-string "};~%")))

(defun c-emit-type (type)
  (cond
    ((eql type 'u64) "unsigned long long")
    ((eql type 's64) "long long")
    ((eql type 'u8) "unsigned char")
    ((eql type 's8) "char")
    ((eql type 'string) "const char*")
    (t "TODO!")))

(defun c-emit-mods (mods)
  (let ((result nil))
  (dolist (m mods result)
    (cond 
      ((eql m 'static) (push "static" result))
      ((eql m 'const) (push "const" result))))))

(defun emit-c-array (array output-string)
  (let* ((body (cdr array))
         (mods (getf body :mods)))
    (when mods
      (dolist (m (c-emit-mods mods))
        (format output-string "~a " m)))
    (format output-string "~a ~a[~a]"
            (c-emit-type (getf body :type))
            (getf body :id)
            (getf body :size))
    (let ((values (getf body :values))
          (i 0))
      (when values
        (format output-string "= {~%")
        (for-list x values
          (let ((v (getf (cdr x) :value)))
            (format output-string "    ~a" v)
            (when (< i (- (length values) 1))
              (format output-string ","))
            (format output-string "~%"))
          (incf i))
        (format output-string "}")))
    (format output-string ";~%")))

(defun emit-c-var (var output-string)
  (let* ((body (cdr var))
         (value (getf body :value)))
    (format output-string "~a ~a"
            (c-emit-type (getf body :type))
            (getf body :id))
    (when value
      (format output-string "= ~a" value))
    (format output-string ";~%")))

(defun emit-c-record (var output-string)
  (let* ((body (cdr var))
         (members (getf body :members)))
    (format output-string "struct ~a {~%" (getf body :id))
    (dolist (m members)
      (case (car m)
        (c-var (emit-c-var m output-string))
        (c-array (emit-c-array m output-string))))
    (format output-string "};~%")))

(defun build-sample-id (path)
  (substitute #\S #\# (pathname-name path)))

(defun build-samples-spec ()
  (mapcar #'(lambda (x) (list :path x :id (build-sample-id x)))
          (directory "Samples/*.wav")))

(defparameter *token->note-number* '(C  0
                                     CS 1
                                     D  2
                                     DS 3
                                     E  4
                                     F  5
                                     FS 6
                                     G  7
                                     GS 8
                                     A  9
                                     AS 10
                                     B  11))

(defparameter *note-number-base* 48)

(defun build-flocks-map ()
  (with-open-file (f "flocks_map.bin"
                     :if-exists :supersede
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (let ((tree (c-tree))
          (members nil)
          (offsets nil)
          (sizes nil)
          (spec (build-samples-spec))
          (midi-map (make-array 128 :initial-element -1 :element-type '(signed-byte 8))))
      (add-samples-to-gen-tree spec f members offsets sizes)

      (let* ((i -1)
             (notes (mapcar
                     #'(lambda (m)
                         (let* ((name (subseq m (length "Flocks_")))
                                (token (read-from-string (remove-if #'digit-char-p name)))
                                (octave (+ *note-number-base*
                                           (* (parse-integer (remove-if
                                                              #'alpha-char-p
                                                              name))
                                              12)))
                                (note-number (+ octave (getf *token->note-number* token))))
                           (list note-number (incf i))))
                     members)))
        (print notes)
        (dolist (n notes)
          (array-put midi-map (car n) (cadr n))))

      (c-tree-append tree (c-enum 'Sample_Map_Key :members (mapcar #'(lambda (m) (c-id m)) members)))
      (c-tree-append tree (c-array 'Sample_Map_Total_Sizes
                                   :type 'u64
                                   :size (length sizes)
                                   :mods '(const static)
                                   :values (mapcar #'(lambda (m) (c-value m)) sizes)))
      (c-tree-append tree (c-array 'Sample_Map_Channel_Sizes
                                   :type 'u64
                                   :mods '(const static)
                                   :size (length sizes)
                                   :values (mapcar #'(lambda (m) (c-value (/ m 2))) sizes)))
      (c-tree-append tree (c-array 'Sample_Map_Offsets
                                   :type 'u64 
                                   :mods '(const static)
                                   :size (length offsets)
                                   :values (mapcar #'(lambda (m) (c-value m)) offsets)))
      (c-tree-append tree (c-array 'Sample_Names
                                   :type 'string 
                                   :mods '(static)
                                   :size (length members)
                                   :values (mapcar #'(lambda (m) (c-value (format nil "\"~a\"" m))) members)))
      (c-tree-append tree (c-array 'Midi_Map
                                   :type 's8
                                   :mods '(const static)
                                   :size (length midi-map)
                                   :values (map 'list #'(lambda (m) (c-value m)) midi-map)))
      (c-tree-append tree (c-record 'Flocks_Sample_Counts
                                    :members (list (c-array 'counts
                                                           :type 'u64
                                                           :size (length sizes)))))
      (c-tree-append tree (c-record 'Flocks_Sample_Voices
                                    :members (list (c-array 'voices
                                                           :type 's64
                                                           :size (length sizes)))))
      (c-tree-append tree (c-var 'u64 'Num_Flocks_Samples :value (length sizes)))
      (let ((output (make-empty-string)))
        (with-output-to-string (s output)
          (format s "#pragma once~%")
          (for-list x tree
            (cond
              ((eql x 'c-syntax) nil)
              ((eql (car x) 'c-enum) (emit-c-enum x s))
              ((eql (car x) 'c-var) (emit-c-var x s))
              ((eql (car x) 'c-record) (emit-c-record x s))
              ((eql (car x) 'c-array) (emit-c-array x s)))
            (format s "~%")))
        (with-open-file (f "flocks_map.h" :direction :output :if-exists :supersede)
          (write-string output f)))
      tree)))

;; We can test these with, this will do one channel at a time:
;; ffmpeg -f f32le -ar 96000 -ac 1 -i test.bin -f pulse default
