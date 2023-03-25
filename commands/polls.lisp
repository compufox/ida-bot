(in-package :ida-bot.extension)

(defvar *current-poll* nil
  "holds our current poll, which is a hashtable

keys are the poll options
values are vote counts")

(defvar *poll-expiry-time* nil
  "time that the current poll expires")

(defvar *poll-started-format*
  "Poll started! Vote with !vote <poll option>

~:{Option ~A: ~A~%~}")

;; defines a new command "!poll" that can only be ran by moderators
;; example usage: !poll 5 option1, option2
;; this starts a poll for 5 minutes with the options "option1" and "option2"
(define-command ("poll" :moderator-only t)
  (setf *current-poll* (make-hash-table :size 4))
  (let* ((command-parms (str:words *command-message*))
         (time-limit (first command-parms))
         (poll-options (loop :with poll-options (str:split #\, (apply #'str:concat (rest command-parms)))
                             :for option :in poll-options
                             :for index :from 1 :upto (length poll-options)
                             :do (setf (gethash index *current-poll*)
                                       (list :option option :votes 0))

                             :collecting (list index option) :into options
                             :finally (return options))))
    (setf *poll-expiry-time* (+ (parse-time time-limit :minutes)
                                (get-universal-time)))
    (send-chat (format nil *poll-started-format* poll-options))))

;; defines a new command "!vote" that can be ran by anyone
;; example usage: !vote 1
;; this applies a single vote for option 1 
(define-command ("vote")
  (let* ((input (first (str:words *command-message*)))
         (option (gethash input *current-poll* nil)))
    (when option  ;; so we dont accidentally vote for an option that doesn't exist
      (setf (gethash input *current-poll*)
            (list :option (getf option :option)
                  :votes (1+ (getf option :votes)))))))


;; defines a new service that checks for when the current poll ends
;; when it does end it calculates the winner and posts the results back into chat
;; if there was no winner then it says as much
(define-service ("poll-result-checker" :start-with-stream t)
  (after-every (2 :seconds)
    (when (and *poll-expiry-time* *current-poll*
               (< *poll-expiry-time* (get-universal-time)))
      (let ((winning-option (loop :with current-max := 0
                                  :with winning-option := ""
                                  
                                  :for v :being :the :hash-value
                                    :of *current-poll*

                                  :when (< current-max (getf v :votes))
                                    :do (setf current-max (getf v :votes)
                                              winning-option (getf v :option))

                                  :finally (return winning-option))))
        (setf *poll-expiry-time* nil
              *current-poll* nil)
        (if (str:emptyp winning-option)
            (send-chat "There was no poll winner :c")
            (send-chat (format nil "Poll ended! Winner is: ~A" winning-option)))))))
                                       
                                 
