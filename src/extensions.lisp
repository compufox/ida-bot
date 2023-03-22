(defpackage ida-bot.extension-loader
  (:use :cl)
  (:import-from :ida-bot.conditions
                :dependency-not-found)
  (:export :load-extensions))
(in-package ida-bot.extension-loader)

(defun load-extensions ()
  (load-directory "./commands/")
  (load-directory "./services/")
  (load-directory "./handlers/"))

(defun load-directory (dir)
  "loads all code from subdirectory"
  (if (uiop:directory-exists-p dir)
      (loop :with needs-reload := nil
            :for file :in (uiop:directory-files dir "*.lisp")
            :do (log:info "Loading file ~A..." file)
                (handler-case (load file)
                  (dependency-not-found (missing-dep)
                    (log:warn "~&Found unmet dependency ~a. Marking file for retry.~%" (ida-bot.conditions:unmet-dependency-id missing-dep))
                    (push file needs-reload)))
            :finally (mapcar #'load needs-reload))
      (log:warn "Directory ~A doesn't exist. Please create it and fill it with extensions~&" dir)))

(in-package :cl-user)
(defpackage ida-bot.extension
  (:use :cl :ida-bot.util :ida-bot.commands
        :ida-bot.actions :ida-bot.config
        :ida-bot.services :ida-bot.handler))
(in-package :ida-bot.extension)
