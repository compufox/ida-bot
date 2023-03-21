(defsystem "ida-bot"
  :version "0.1.0"
  :author "a. fox"
  :license ""
  :depends-on ("bordeaux-threads"
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
                ((:file "main" :depends-on ("web" "config" "commands"))
                 (:file "web" :depends-on ("util" "commands"))
                 (:file "commands" :depends-on ("util" "owncast"))
                 (:file "owncast" :depends-on ("util" "config"))
                 (:file "util")
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "ida-bot-test")))
  :build-operation "program-op"
  :build-pathname "bin/ida-bot"
  :entry-point "ida-bot::make-start")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
