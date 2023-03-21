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
           :productionp))
(in-package :ida-bot.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :ida-bot))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))

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
      (conf:config key default)))
