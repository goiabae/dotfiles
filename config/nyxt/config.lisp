(in-package #:nyxt-user)

(define-configuration :context-buffer
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
