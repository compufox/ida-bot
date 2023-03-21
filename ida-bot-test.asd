(defsystem "ida-bot-test"
  :defsystem-depends-on ("prove-asdf")
  :author "a. fox"
  :license ""
  :depends-on ("ida-bot"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "ida-bot"))))
  :description "Test system for ida-bot"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
