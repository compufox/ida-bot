(defpackage ida-bot.moderator
  (:use :cl)
  (:export moderator-p))
(in-package :ida-bot.moderator)

(defvar *moderator-list* nil
  "list of moderators")

(defun moderator-p (user-id)
  (member user-id *moderator-list* :test #'equal))
