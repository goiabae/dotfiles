(setq visible-cursor nil)
(column-number-mode)
(show-paren-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 4) ; Give some breathing room
(menu-bar-mode -1)
(blink-cursor-mode 0)
; (set-default-font "Fira Code-12")
(set-frame-parameter nil 'undecorated t)

(setq electric-pair-skip-self t)
(setq electric-pair-pairs
			'((?\{ . ?\})
				(?\(. ?\))
				(?\[ . ?\])
				(?\< . ?\>)
				(?\" . ?\")))
(electric-pair-mode)

(use-package hl-todo
	:straight t
	:hook (prog-mode . hl-todo-mode))

(custom-set-faces
 '(org-level-1 ((t (:inherit default :height 1.5)))) ; red
 '(org-level-2 ((t (:inherit default :height 1.4)))) ; orange
 '(org-level-3 ((t (:inherit default :height 1.32)))) ; yellow
 '(org-level-4 ((t (:inherit default :height 1.24))))
 '(mini-modeline-mode-line
	 ((((background light)) :background "#458588" :height 0.14 :box nil)
		(t (:inherit default :height 0.14 :box nil :background "#79740e")))))

;; load theme
(use-package dash
			 :straight t)
(use-package autothemer
			 :straight t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
; (load-theme 'gruvbox-light-hard t)
(load-theme 'battery t)

(use-package mini-modeline
	:straight t
	:config
	(setq mini-modeline-right-padding 2)
	(setq mini-modeline-face-attr '(:inherit default)))
  (mini-modeline-mode t)

;; uncomment to have all braces pairs with matching rainbow colors
(use-package rainbow-delimiters
	:straight t
	:hook (prog-mode . rainbow-delimiters-mode))

(setq calendar-latitude -23.61)
(setq calendar-longitude -46.65)

(use-package theme-changer :straight t)
(require 'theme-changer)
;; relay it by 30 minutes so that i changes at 18:30 rather than 19:00
(setq theme-changer-delay-seconds (- 1800))
(if (not (string-equal system-name "nikaido"))
    (change-theme 'gruvbox-light-hard 'battery))

(setq org-format-latex-options
      '(:foreground default
                    :background default
                    :scale 1.4
                    :html-foreground "Black"
                    :html-background "Transparent"
                    :html-scale 1.0
                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-startup-with-latex-preview t)
(load "~/.emacs.d/source/org-fragtog.el" t)
(require 'org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)
(setq frame-resize-pixelwise t)
