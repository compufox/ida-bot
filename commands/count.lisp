(in-package :ida-bot.extension)

(unless (uiop:file-exists-p "stab.count")
  (str:to-file "stab.count" "0"))

(defvar *stab-count* (parse-integer (str:from-file "stab.count")))

(define-command ("stab")
  (incf *stab-count*)
  (str:to-file "stab.count" (format nil "~A" *stab-count*))
  (send-chat (format nil "Ceasar has been stabbed ~A times" *stab-count*)))
   
  
  
