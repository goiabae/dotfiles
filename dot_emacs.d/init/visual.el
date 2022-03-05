(setq visible-cursor nil)
(column-number-mode)
(show-paren-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 4) ; Give some breathing room
;; (menu-bar-mode -1)
; (set-default-font "Fira Code-12")

(setq electric-pair-pairs
			'((34 . 34)
				(8216 . 8217)
				(8220 . 8221)
				(?\< . ?\>)))
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
(change-theme 'gruvbox-light-hard 'battery)
