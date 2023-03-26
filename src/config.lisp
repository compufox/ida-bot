(in-package :cl-user)
(defpackage ida-bot.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :env
           :*application-root*
           :*static-directory*
           :appenv
           :developmentp
           :productionp
           :load-config
           :config-stale-p))
(in-package :ida-bot.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :ida-bot))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defvar *config-file* nil)
(defvar *loaded-config* nil)
(defvar *config-modified-time* nil
  "variable that contains the last modified time for the config file")

(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))

(defun env (key &optional default)
  (or (config key)
      (conf:config key default *loaded-config*)))

(defun config-stale-p ()
  (< *config-modified-time* (file-write-date *config-file*)))

(defun load-config (&optional file)
  (when file (setf *config-file* file))
  
  (handler-case 
      (let ((alst (conf:load-config (or file *config-file*) :parse-lists nil :set-internal-var nil)))
        (when alst
          (setf *config-modified-time* (file-write-date (or file *config-file*)))
          (setf *loaded-config* alst)))
    (error (e)
      (log:error "Encountered error loading config file: ~A~%" e))))
