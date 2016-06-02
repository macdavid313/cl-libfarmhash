#|
  This file is a part of cl-libfarmhash project.
  Copyright (c) 2016 David Gu (david_guru@gty.org.in)
|#

(in-package :cl-user)
(defpackage cl-libfarmhash-test-asd
  (:use :cl :asdf))
(in-package :cl-libfarmhash-test-asd)

(defsystem cl-libfarmhash-test
  :author "David Gu"
  :license ""
  :depends-on (:cl-libfarmhash
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-libfarmhash"))))
  :description "Test system for cl-libfarmhash"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
