(in-package :ida-bot.extension)

(defvar *banned-words* (str:split #\, (env :automod-banned-words))
  "a list containing our banned words pulled from our config")

(defvar *logged-messages* (make-hash-table :size 20 :rehash-size 2.5
                                           :test 'equal)
  "hash table full of messages we've moderated

hash key is userId
hash value is list of moderated chat ids")

(define-handler ("automod" :priority most-negative-fixnum)
  ;; this is where we need to search the incoming message
  ;; for any words in our no-no list :weary:
  (let ((user (agetf *handler-data* "user")))
    (loop :for word :in (str:split #\Space (agetf *handler-data* "body"))
          :when (member word *banned-words* :test #'string=)
            :do
               (moderate-chat (agetf *handler-data* "id") nil)
               (push (agetf *handler-data* "id")
                     (gethash (agetf user "id") *logged-messages*))
               #|
               (setf (gethash (agetf user "id") *logged-messages*)
                     (append (gethash (agetf user "id") *logged-messages*)
                             (list (agetf *handler-data* "id"))))
                 |#    
               (return))))
