(defpackage ida-bot.extension-loader
  (:use :cl)
  (:export :load-extensions))
(in-package ida-bot.extension-loader)

(defun load-extensions ()
  (load-directory "./commands/")
  (load-directory "./services/")
  (load-directory "./handlers/"))

(defun load-directory (dir)
  "loads all code from subdirectory"
  (if (uiop:directory-exists-p dir)
      (loop :for file :in (uiop:directory-files dir "*.lisp")
        :do (format t "Loading file ~A~%" file)
            (load file))
      (format t "Directory ~A doesn't exist. Please create it and fill it with extensions~&" dir)))

(in-package :cl-user)
(defpackage ida-bot.extension
  (:use :cl :ida-bot.util :ida-bot.commands
        :ida-bot.actions :ida-bot.config
        :ida-bot.services :ida-bot.handler))
(in-package :ida-bot.extension)
