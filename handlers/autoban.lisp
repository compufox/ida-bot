(in-package :ida-bot.extension)

(defvar *user-strikes* (make-hash-table :test 'equal)
  "a hashtable representing how many times each user has had their message caught by the automod")

(define-handler ("autoban" :type :visibility-update :depends-on "automod")
  (unless (agetf *handler-data* "visible")
    (loop :for user-id :being :the :hash-key
            :using (hash-value moderated-messages) :of *logged-messages*

          :do (loop :for id :in (agetf *handler-data* "ids")
                    :when (member id moderated-messages :test #'string=)
                      :do (let ((user-strikes (1+ (gethash user-id *user-strikes* 0))))
                            (setf (gethash user-id *user-strikes*) user-strikes)
                            (when (<= (env :autoban-strike-count 5)
                                      user-strikes)
                              (moderate-user user-id nil)))))))
