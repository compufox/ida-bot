(defpackage ida-bot.routes
  (:use :cl :cl-markup :caveman2 :parenscript)
  (:import-from :ida-bot.web :*web*)
  (:export :define-media-popup))
(in-package :ida-bot.routes)

;; maybe add in a timeout option?
(defmacro define-media-popup (route media-path)
  "defines a new ROUTE that will display a video at MEDIA-PATH, when the trigger-function is called

returns TRIGGER-FUNCTION

ROUTE is the web route that the video will be played at
VIDEO-PATH is the web route to the video file - should be located in the bot's *static-directory* (defaults to ./static/)"
  (let ((trigger-var (gensym)))
    `(let ((,trigger-var))
       (declare (ignorable ,trigger-var))
       (prog1
           (lambda ()
             (unless ,trigger-var
               (setf ,trigger-var t)))
         (defroute (*web* ,route :method :GET) ()
           (prog1 (html5
                   (:head
                    (:meta :http-equiv "refresh" :content (if ,trigger-var "-1" "1"))
                    (:script :type "text/javascript"
                             (raw "
function hidePopup () {
  var element = document.getElementById(\"popup\");
  element.style = \"display:none;\";
  window.location.reload();
}

function displayPopup () {
  var element = document.getElementById(\"popup\");
  element.style = \"display:block\";
  element.play();
}")))
                   (:body :onload (if ,trigger-var "displayPopup();" "")
                          (:video :id "popup" :style "display:none;" :preload "auto" :onended "hidePopup();"
                                  (:source :src ,media-path))))
             (when ,trigger-var (setf ,trigger-var nil))))))))
                                

