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
                              ;; TODO: once integrations can properly ban users
                              ;;  the following line can be removed and the
                              ;;  commented out line can be used.
                              (log:info "User ~A has been flagged multiple times and should be banned" user-id)))))))
                              ;;(moderate-user user-id nil)))))))
