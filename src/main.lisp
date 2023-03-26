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
  (:use :cl :with-user-abort :ida-bot.util)
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
      (quit-app 0 "ida-bot v~A~&" #.(asdf:component-version (asdf:find-system :ida-bot))))
    
    (when (getf opts :help)
      (unix-opts:describe
       :usage-of "idabot")
      (quit-app 0))
    
    (when (getf opts :prod)
      (setf (uiop:getenv "APP_ENV") "production"))

    (if (getf opts :config)
        (if (uiop:file-exists-p (getf opts :config))
            (ida-bot.config:load-config (getf opts :config))
            (quit-app 1 "Specified config file does not exist.~&"))
        (quit-app 1 "Please specify a config file.~&"))

    ;; load all of our extensions either from the
    ;; user specified directory or the standard "./extensions" directory
    (ida-bot.extension-loader:load-extensions
     (getf opts :extension-directory "./extensions/"))

    ;; if the stream is currently going, make sure
    ;; to start ALL services. otherwise we just
    ;; start the ones that dont need the stream to be up and running
    (if (agetf (get-current-status) "online")
        (ida-bot.services:start-services :all t)
        (ida-bot.services:start-services))

    ;; load the saved list of moderator ids
    (ida-bot.moderator:load-moderator-list)
  
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

      ;; here we need to clean up a lil...
      
      ;; this stops the actual webserver 
      (stop)
      
      ;; this stops all of our background threads
      (ida-bot.services:stop-services :all t)

      ;; this ensures we save our list of moderator user ids
      (ida-bot.moderator:save-moderator-list))))
