(defpackage ida-bot.extension-loader
  (:use :cl)
  (:import-from :ida-bot.conditions
                :dependency-not-found)
  (:export :load-extensions))
(in-package ida-bot.extension-loader)

(defun load-extensions ()
  (mapcar #'load-directory
          '("./commands/" "./services/" "./handlers/")))

(defun load-directory (dir)
  "loads all code from subdirectory"
  (if (uiop:directory-exists-p dir)
      ;; Attempt to load any lisp files in DIR
      ;; if we encounter a (catchable) issue 
      (mapcar #'load-file
              (remove-if #'identity
                         (mapcar #'load-file (uiop:directory-files dir "*.lisp"))))
      (log:warn "Directory ~A doesn't exist. Please create it and fill it with extensions~&" dir)))

(defun load-file (file)
  (log:info "Loading file ~A..." file)
  (handler-case (load file)
    (dependency-not-found (missing-dep)
      (log:warn "!!!Found unmet dependency '~a'. Marking file for retry.~&" (ida-bot.conditions:unmet-dependency-id missing-dep))
      file)))

(in-package :cl-user)
(defpackage ida-bot.extension
  (:use :cl :ida-bot.util :ida-bot.commands
        :ida-bot.actions :ida-bot.config
        :ida-bot.services :ida-bot.handler))
(in-package :ida-bot.extension)
