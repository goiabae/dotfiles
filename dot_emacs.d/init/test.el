;; (setq-default mode-line-format
;; 							'("%e" mode-line-front-space
;; 							 mode-line-mule-info
;; 							 mode-line-client
;; 							 mode-line-modified
;; 							 mode-line-remote
;; 							 mode-line-frame-identification
;; 							 " hello from the other siiide " mode-line-buffer-identification "   " mode-line-position
;; 							 (symbol-name major-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote ("%e "
             mode-line-buffer-identification
             " %l : %c"
             evil-mode-line-tag
             "[%*]"))
     ;; Right.
     (quote ("%p "
             mode-line-frame-identification
             mode-line-modes
             mode-line-misc-info))))))

	 
