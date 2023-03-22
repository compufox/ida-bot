(defpackage ida-bot.extension
  (:use :cl :ida-bot.util :ida-bot.commands
        :ida-bot.actions :ida-bot.config
        :ida-bot.services :ida-bot.handlers)
  (:export :load-commands
           :load-services))
(in-package :ida-bot.extension)

(defun load-commands ()
  (load-directory "./commands/"))

(defun load-services ()
  (load-directory "./services/"))

(defun load-directory (dir)
  "loads all code from subdirectory"
  (if (uiop:directory-exists-p dir)
      (loop :for file :in (uiop:directory-files dir "*.lisp")
        :do (format t "Loading file ~A~%" file)
            (load file))
      (format t "Directory ~A doesn't exist. Please create it and fill it with extensions~&" dir)))

  
