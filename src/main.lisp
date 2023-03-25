(defpackage ida-bot.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :ida-bot.web
                :*web*)
  (:import-from :ida-bot.config
                :env
                :productionp
                :*static-directory*)
  (:export :get-builder))
(in-package :ida-bot.app)

(defun get-builder ()
  (builder
   nil
   (if (productionp)
       nil
       :accesslog)
   (if (env :error-log)
       `(:backtrace
         :output ,(env :error-log))
       nil)
   :session
   (if (productionp)
       nil
       nil)
   *web*))

;; 
;; main app package
;;

(in-package :cl-user)
(defpackage ida-bot
  (:use :cl :with-user-abort)
  (:import-from :ida-bot.config
   :env)
  (:import-from :ida-bot.actions
                :get-current-status)
  (:import-from :clack
   :clackup)
  (:import-from :unix-opts
   :define-opts
   :get-opts)
  (:export :start
           :stop))
(in-package :ida-bot)

(define-opts
  (:name :help
   :description "prints this help text"
   :short #\h
   :long "help")
  (:name :port
   :description "sets port"
   :short #\p
   :long "port"
   :arg-parser #'parse-integer
   :meta-var "PORT")
  (:name :config
   :description "specify the config to use"
   :short #\c
   :long "config"
   :arg-parser #'string
   :meta-var "FILE")
  (:name :prod
   :description "enabled production mode"
   :long "production")
  (:name :version
   :description "prints application version"
   :long "version")
  (:name :extension-directory
   :description "specifies where to look for extensions"
   :short #\d
   :long "extension-dir"
   :arg-parser #'string
   :meta-var "DIRECTORY"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup (ida-bot.app:get-builder) args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defun make-start ()
  "our binary entry point"
  (multiple-value-bind (opts args) (get-opts)
    (when (getf opts :version)
      (format t "ida-bot v~A~&" #.(asdf:component-version (asdf:find-system :ida-bot)))
      (uiop:quit 0))
    
    (when (getf opts :help)
      (unix-opts:describe
       :usage-of "idabot")
      (uiop:quit 0))
    
    (when (getf opts :prod)
      (setf (uiop:getenv "APP_ENV") "production"))

    (if (getf opts :config)
        (conf:load-config (getf opts :config) :parse-lists nil)
        (progn
          (format t "please specify a config file to use")
          (uiop:quit 1)))
  
    (ida-bot.extension-loader:load-extensions
     (getf opts :extension-directory "./extensions/"))

    (if (agetf (get-current-status) "online")
        (ida-bot.services:start-services :all t)
        (ida-bot.services:start-services))
  
    (let ((server #+(and Unix SBCL) :woo
                  #-(and Unix SBCL) :hunchentoot))

      (log:info "Starting bot...")
      
      (start :port (env :port (or (getf opts :port) 9000)) :server server)
    
      (handler-case
          (with-user-abort
            (bt:join-thread (find-if (lambda (th)
                                       (search (string-downcase (string server)) (bt:thread-name th)))
                                     (bt:all-threads))))
        (user-abort ())
        (error (c) (log:error "Woops, an unknown error occured:~&~a~&" c)))
    
      (log:info "~&Quitting bot.")
      (stop)
      (ida-bot.services:stop-services :all t))))
