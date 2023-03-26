(defsystem "ida-bot"
  :version "0.1.0"
  :author "a. fox"
  :license ""
  :depends-on ("bordeaux-threads"
               "log4cl"
               "clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
               "str"
               #+(and Unix SBCL) "woo"
               "com.inuoe.jzon"
               "alexandria"

               ;; for @route annotation
               "cl-syntax-annot"
               "with-user-abort"

               "simple-config"
               "unix-opts"
               "drakma")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("web" "config" "commands" "extensions"
                                            "owncast" "util"))
                 (:file "extensions" :depends-on ("owncast" "services" "commands"
                                                  "util" "config" "web" "handlers"
                                                  "conditions"))
                 (:file "web" :depends-on ("util" "commands" "handlers"
                                           "services"))
                 (:file "owncast" :depends-on ("util" "config"))
                 (:file "services" :depends-on ("util" "conditions"))
                 (:file "handlers" :depends-on ("util" "conditions"))
                 (:file "commands" :depends-on ("util" "owncast"))
                 (:file "util" :depends-on ("conditions"))
                 (:file "conditions")
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "ida-bot-test")))
  :build-operation "program-op"
  :build-pathname "bin/ida-bot"
  :entry-point "ida-bot::make-start")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
