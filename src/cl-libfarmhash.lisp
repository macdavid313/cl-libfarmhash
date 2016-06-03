;;;; cl-libfarmhash.lisp
(in-package #:cl-user)
(defpackage #:cl-libfarmhash
  (:use #:cl)
  (:import-from :cffi
                #:foreign-funcall
                #:defcstruct
                #:defctype
                #:defcfun
                #:with-foreign-object
                #:foreign-slot-value
                #:translate-from-foreign
                #:with-foreign-string)
  (:nicknames #:libfarmhash #:farmhash #:farm)
  (:export #:farmhash
           #:farmhash32 #:farmhash32-with-seed           
           #:farmhash64 #:farmhash64-with-seed #:farmhash64-with-seeds
           #:farmhash128 #:farmhash128-with-seed
           #:farmhash-fingerprint32 #:farmhash-fingerprint64 #:farmhash-fingerprint128))
(in-package #:cl-libfarmhash)

;;; Compile and Load External C Module
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *root-path*
    (uiop:pathname-directory-pathname
     (asdf:system-definition-pathname :cl-libfarmhash)))
  
  (defvar *ext-path* (merge-pathnames "ext/" *root-path*))
  
  (defvar *cc* (or (uiop:getenv "CC") "cc"))

  (uiop:with-current-directory (*ext-path*)
    (let ((command (reduce (lambda (x y)
                             (concatenate 'string x " " y))
                           (list *cc*
                                 #+x86 "-m32" #+(or x86_64 x86-64) "-m64"
                   "-fpic -shared farmhash.c -o"
                   #+(or darwin macos macosx) "libfarmhash.dylib"
                   #-(or darwin macos macosx) "libfarmhash.so"))))
      (format t "~%~A~%" command)
      (uiop:run-program
       command
       :output t
       :error-output t
       :ignore-error-status t)))
    
  (pushnew *ext-path* cffi:*foreign-library-directories* :test 'equal)
  
  (cffi:define-foreign-library libfarmhash
    (t (:default "libfarmhash")))

  (unless (cffi:foreign-library-loaded-p 'libfarmhash)
    (with-simple-restart
        (skip "Skip loading libfarmhash.")
      (cffi:use-foreign-library libfarmhash))))

;;; C types
(defctype size-t :unsigned-int)

(defcstruct (uint128 :size 16)
  (a :uint64) ;; low64
  (b :uint64)) ;; high64

(defctype uint128 (:struct uint128))

(defmethod translate-from-foreign (ptr (type uint128-tclass))
  "Translate a c struct, `uint128`, to a Lisp integer."
  (let ((a (foreign-slot-value ptr 'uint128 'a)) ;; low64
        (b (foreign-slot-value ptr 'uint128 'b))) ;; high64
    (logior (ash b 64) a)))

(defmacro define-farmhash-function
    (name return-type docstring &rest other-args)
  "A macro that helps define Farmhash API functions."
  (let ((c-name (if (consp name) (car name) name))
        (lisp-name (if (consp name) (cadr name)
                       (case (readtable-case *readtable*)
                         (:preserve (intern name))
                         (t (intern (string-upcase name))))))
        (str (gensym "CSTR"))
        (other-args-cells-list
         (when other-args
           (loop for lst = other-args then (cddr lst)
              with result = nil
              while lst do (push (list (car lst) (cadr lst)) result)
              finally (return (nreverse result))))))
    `(defun ,lisp-name
         ,(if other-args
              `(string ,@(mapcar 'second other-args-cells-list))
              '(string))
       ,docstring
       (declare (type simple-string string)
                (optimize speed (space 0) (safety 0)))
       (with-foreign-string (,str string)
         ,(if other-args
              `(foreign-funcall ,c-name
                                :pointer ,str
                                size-t (length string)
                                ,@other-args
                                ,return-type)
              `(foreign-funcall ,c-name
                                :pointer ,str
                                size-t (length string)
                                ,return-type))))))

(define-farmhash-function "farmhash" size-t
  "Hash function for a byte array.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG.")

(define-farmhash-function "farmhash32" :uint32
  "Hash function for a byte array. Most useful in 32-bit binaries.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG.")


(define-farmhash-function
    ("farmhash32_with_seed" farmhash32-with-seed) :uint32
  "Hash function for a byte array. For convenience, a 32-bit seed is also hashed into the result.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG."
  :uint32 seed)

(define-farmhash-function "farmhash64" :uint64
  "Hash 128 input bits down to 64 bits of output.
Hash function for a byte array.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG.")

(define-farmhash-function
    ("farmhash64_with_seed" farmhash64-with-seed) :uint64
  "Hash function for a byte array. For convenience, a 64-bit seed is also hashed into the result.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG."
  :uint64 seed)

(define-farmhash-function
    ("farmhash64_with_seeds" farmhash64-with-seeds) :uint64
  "Hash function for a byte array. For convenience, two seeds are also hashed into the result.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG."
  :uint64 seed0 :uint64 seed1)

(define-farmhash-function "farmhash128" uint128
  "Hash function for a byte array.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG.")

(declaim (inline split-128bit-number-to-64bit))
(defun split-128bit-number-to-64bit (number)
  "Given a number, retruns its low 64bit and high 64bit."
  (let ((low64-mask #x0000000000000000ffffffffffffffff)
        (high64-mask #xffffffffffffffff0000000000000000))
    (values (logand number low64-mask) ;; low64
            (ash (logand number high64-mask) -64)))) ;; high64

(defun farmhash128-with-seed (string seed)
  "Hash function for a byte array. For convenience, a 128-bit seed is also hashed into the result.
May change from time to time, may differ on different platforms, may differ depending on NDEBUG."
  (declare (type simple-string string))
  (with-foreign-object (c_seed 'uint128)
    (multiple-value-bind (low high) (split-128bit-number-to-64bit seed)
      (setf (foreign-slot-value c_seed 'uint128 'a) low
            (foreign-slot-value c_seed 'uint128 'b) high)
      (with-foreign-string (str string)
        (foreign-funcall "farmhash128_with_seed"
                         :pointer str
                         size-t (length string)
                         :pointer c_seed
                         uint128)))))

(define-farmhash-function ("farmhash_fingerprint32" farmhash-fingerprint32) :uint32
  "Fingerprint function for a byte array. Most useful in 32-bit binaries.")

(define-farmhash-function ("farmhash_fingerprint64" farmhash-fingerprint64) :uint64
  "Fingerprint function for a byte array.")

(define-farmhash-function ("farmhash_fingerprint128" farmhash-fingerprint128) uint128
  "Fingerprint function for a byte array.")
