(in-package :ida-bot.extension)

(defvar *loaded-extensions* nil
  "list of all our currently loaded extensions")

(let ((opts (opts:get-opts)))
  ;; ensures that when we load this extension
  ;; we cache all of our extensions right off the bat
  (setf *loaded-extensions* (uiop:directory-files (getf opts :extension-directory "./extensions/") "*.lisp"))

  
  ;; defines a service that checks for new extensions placed in the user's extension directory
  ;; if it sees an extension that isn't loaded it loads it.
  ;;
  ;; by default checks every 5 minutes
  ;; can be custmoized by setting autoloader-timeout in your config (see config.example)
  (define-service ("extension-autoloader" :start-immediately t)
    (after-every ((env :autoloader-timeout 5) :minutes)
      (loop :for file :in (uiop:directory-files (getf opts :extension-directory "./extensions/") "*.lisp")
            :unless (member file *loaded-extensions* :test #'equal)
              :do (load file)
                  (push file *loaded-extensions*)))))
