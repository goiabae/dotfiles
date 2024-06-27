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
         :fallback-url (quri:uri "https://duckduckgo.com/"))))))

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
