(in-package :cl-user)
(defpackage ida-bot.web
  (:use :cl
        :caveman2)
  (:import-from :ida-bot.util
   :agetf)
  (:import-from :ida-bot.commands
   :process-commands)
  (:import-from :ida-bot.handler
   :run-handlers
   :define-handler)
  (:export :*web*))
(in-package :ida-bot.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Handler that ensures we know who our moderators
(define-handler ((string (gensym "MODERATOR-TRACKER")) :type :user-joined)
    (let ((user-id (agetf (agetf *handler-data* "user") "id")))
      (when (and (member "MODERATOR" (agetf *handler-data* "scopes") :test #'equal)
                 (not (moderator-p user-id)))
        (push user-id ida-bot.moderator::*moderator-list*))))

;;
;; Routing rules

@route POST "/webhook"
(defun parse-webhooks (&key _parsed)
  (run-handlers _parsed)
  (process-commands _parsed))
  

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
