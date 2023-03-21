(in-package :cl-user)
(defpackage ida-bot.web
  (:use :cl
        :caveman2)
  (:import-from :ida-bot.util
   :agetf)
  (:import-from :ida-bot.commands
   :process-commands)
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
;; Routing rules

@route POST "/ingest"
(defun parse-webhooks (&key _parsed)
  (process-commands _parsed))
  

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
