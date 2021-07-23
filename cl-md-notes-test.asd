(defsystem "cl-md-notes-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-md-notes"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-md-notes"))))
  :description "Test system for cl-md-notes"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
