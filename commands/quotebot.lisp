(in-package :ida-bot.extension)

;; load a SQLite3 library from Quicklisp
(ql:quickload :sqlite)

;; create our database if it doesnt exist
;; and initialize the database schema
(unless (uiop:file-exists-p "quotes.db")
  (with-open-file (out "quotes.db" :direction :output :if-does-not-exist :create))
  (sqlite:with-open-database (db "quotes.db")
    (sqlite:execute-non-query db "create table quotes (id integer primary key, user text not null, quote text not null)")))

(defun quotes-get-random-user ()
  (sqlite:with-open-database (db "quotes.db")
    (let ((users (sqlite:execute-to-list db "select unique(user) from quotes")))
      (alexandria:random-elt users))))

;; creates a command to add a new quote
(define-command ("addquote")
  (let* ((msg (str:words *command-message*))
         (user (first msg))
         (quote (str:unwords (rest msg))))
    (sqlite:with-open-database (db "quotes.db")
      (sqlite:execute-non-query/named db "insert into quotes (user, quote) values (:user, :quote)"
                                      ":user" user ":quote" quote))
    (send-chat "Quote added!")))

;; creates a command that will retrieve a random quote from our DB
;;  if we get a username we pull a random quote from that user,
;;  otherwise we fetch a random username and use it to get a quote
(define-command ("quote")
    (sqlite:with-open-database (db "quotes.db")
      (let* ((user (if (str:emptyp *command-message*)
                       (quotes-get-random-user)
                       (first (str:words *command-message*))))
             (quotes (sqlite:execute-to-list db "select quote from quotes where user = ?" user))
             (quote (alexandria:random-elt quotes)))
        (send-chat* "**~A**: ~A" user quote))))
