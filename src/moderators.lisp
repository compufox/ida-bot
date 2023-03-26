(defpackage ida-bot.moderator
  (:use :cl)
  (:export
   :moderator-p
   :load-moderator-list
   :save-moderator-list))
(in-package :ida-bot.moderator)

(defvar *moderator-list* nil
  "list of moderators")

(defvar *moderator-file* "./moderator.list")

(defun moderator-p (user-id)
  (member user-id *moderator-list* :test #'equal))

(defun load-moderator-list ()
  (when (uiop:file-exists-p *moderator-file*)
    (setf *moderator-list*
          (str:split #\, (str:from-file *moderator-list*)))))

(defun save-moderator-list ()
  (with-open-file (mod-file *moderator-file*
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (format mod-file "~{~A~^,~}" *moderator-list*)))
