(defsystem :vp-trees
    :name :vp-trees
    :version "1.0"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Perceptual hash algorithms for images"
    :license "2-clause BSD"
    :serial t
    :components ((:file "src/package")
                 (:file "src/vp-trees"))
    :depends-on (:serapeum :float-features)
    :in-order-to ((test-op (load-op "vp-trees/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :vp-trees-tests))))))

(defsystem :vp-trees/tests
    :name :vp-trees/tests
    :version "1.0"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :components ((:file "tests/package")
                 (:file "tests/tests" :depends-on ("tests/package")))
    :depends-on (:vp-trees :fiveam))
