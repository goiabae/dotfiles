;; written for Nyxt 3.11

(in-package #:nyxt-user)

(define-configuration context-buffer
  ((search-engines
     (list
       (make-instance 'search-engine
         :name "Wikipedia"
         :shortcut "wiki"
         :search-url "https://en.wikipedia.org/w/index.php?search=~a"
         :fallback-url (quri:uri "https://en.wikipedia.org/"))
       (make-instance 'search-engine
         :name "DuckDuckGo"
         :shortcut "ddg"
         :search-url "https://duckduckgo.com/?q=~a"
         :fallback-url (quri:uri "https://duckduckgo.com/"))
       (make-instance 'search-engine
         :name "NüschtOS Search"
         :shortcut "nscht"
         :search-url "https://search.nüschtos.de/?query=~a"
         :fallback-url (quri:uri "https://search.nüschtos.de/"))
       (make-instance 'search-engine
         :name "Noogle"
         :shortcut "ngle"
         :search-url "https://noogle.dev/q?term=~a"
         :fallback-url (quri:uri "https://noogle.dev/"))))))

(define-configuration document-buffer
  ((zoom-ratio-default 0.8)))

(define-configuration status-buffer
  ((height 18)))

(define-configuration window
  ((message-buffer-height 14)))

(define-configuration panel-buffer
  ((zoom-ratio-default 0.7)))

(define-configuration input-buffer
  ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration prompt-buffer
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration web-buffer
  ((default-modes
    (append %slot-value%
      '(nyxt/mode/reduce-tracking:reduce-tracking-mode)))))

(defvar battery-theme
  (make-instance 'theme:theme
    :background-color "#fef5d8"
    :secondary-color "#bdae93"
    :text-color "#262524"
    :action-color- "#d67146"
    :action-color "#d67146"
    :action-color+ "#d67146"
    :primary-color "#635852"
    :codeblock-color "#e7d8b3"
    :background-color- "#f2e4ba"
    :background-color+ "#fcf7e8"))

(define-configuration browser
  ((theme battery-theme)))

(defvar +mpv-command+ "mpv")

(defun open-yt-in-mpv-handler (request-data)
  "Open YouTube videos in MPV using `+mpv-command+'."
  (let* ((url (url request-data))
         (is-watch-route
           (and url
             (member (quri:uri-domain url) '("youtube.com" "youtu.be")
               :test #'string=)
             (string= (quri:uri-path url) "/watch"))))
    (if is-watch-route
      (progn
        (echo "Youtube: opening video in mpv ~a" url)
        (uiop:launch-program (list +mpv-command+ (quri:render-uri url)))
        nil)
      request-data)))

(define-configuration web-buffer
  ((request-resource-hook
    (hooks:add-hook %slot-default% 'open-yt-in-mpv-handler))))
