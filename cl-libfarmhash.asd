#|
  This file is a part of cl-libfarmhash project.
  Copyright (c) 2016 David Gu (macdavid313@gmail.com)
|#

#|
  Common Lisp Binding for Google's Farmhash.

  Author: David Gu (macdavid313@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-libfarmhash-asd
  (:use :cl :asdf))
(in-package :cl-libfarmhash-asd)

(defsystem cl-libfarmhash
  :version "0.1"
  :author "David Gu"
  :license "MIT"
  :depends-on (#-asdf3 :uiop
               :cffi :cffi-libffi)
  :components ((:module "src"
                :components
                ((:file "cl-libfarmhash"))))
  :description "Common Lisp Binding for Google's Farmhash."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
