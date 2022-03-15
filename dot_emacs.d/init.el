(setq gc-cons-threshold (expt 2 23))
(add-hook 'emacs-startup-hook
					(lambda ()
						(message
						 "*** Emacs loaded in %s seconds with %d garbage collections."
						 (emacs-init-time)
						 gcs-done)))

(when (fboundp 'native-compile-async)
	(setq comp-deferred-compilation t
				comp-deferred-compilation-blacklist '("/mu4e.*\\.el$")))

(setq user-emacs-directory "~/.emacs.d/")
(when (file-exists-p "/home/goiabae/.emacs.d/custom.el")
	(load "/home/goiabae/.emacs.d/custom.el"))

(load "~/.emacs.d/init/utils.el")

(setq inhibit-startup-message t
			initial-major-mode 'org-mode)

;; tabs can go to hell
(setq indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)

;; enable visual bell on return begginign of line
; (setq visible-bell t)

;; straight.el stuff
(defvar bootstrap-version)
(let ((bootstrap-file
			 (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory))
			(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
				(url-retrieve-synchronously
				 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defvar modules-separator)
(setq modules-separator "+")

(use-package which-key
	:straight t
	:config
	(which-key-mode))

(bind-keys
 ("C-c e r" . reload-config)
 ("C-x w h" . windmove-left)
 ("C-x w j" . windmove-down)
 ("C-x w k" . windmove-up)
 ("C-x w l" . windmove-right)
 ("C-x w d" . delete-window)
 ("C-x w s" . split-window-below)
 ("C-x w v" . split-window-right)
 ("C-x F s" . save-buffer)
 ("C-x F o" . find-file)
 ("C--"     . text-scale-decrease)
 ("C-="     . text-scale-increase)
 ("C-x T l" . display-line-numbers-mode)
 ("C-x B s" . switch-to-buffer)
 ("C-x B k" . kill-buffer)
 ("C-x j t" . journal-open-today))

(use-package meow
	:straight t
	:config
	(meow-setup)
	(setq meow-use-clipboard t)
	(meow-global-mode 1))

(setq-default tab-width 2)

(setq org-adapt-indentation nil
			org-src-preserve-indentation t)

(require 'org-tempo) ; < s TAB => insert source block

(use-package vertico
	:straight t
	:ensure t
	:init
	(vertico-mode)
	:config
	(setq vertico-count 6))

;; manually load vertico-grid because `use-package` was having issues
(load "~/.emacs.d/source/vertico-grid.el" t)
(require 'vertico-grid)
(vertico-grid-mode 1)

(use-package company
	:straight t
	:config
	(setq company-selection-wrap-around t)
	(setq company-minimum-prefix-length 1)
	(company-tng-configure-default))

(add-hook 'prog-mode-hook 'global-company-mode)

;; use primary clipboard for yanking
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)

;; load modules
(defun load-modules (modules)
  (mapcar
   (lambda (a)
	    (load (concat
						 "~/.emacs.d" "/init/"
					a ".el")))
		modules))

;; (let ((a (getenv "EMACS_SESSION")))
;; 	(when (not (= (length a) 0))
;; 		(load-modules
;; 		 (split-string a modules-separator))))

(setq make-backup-files nil)
; (put 'downcase-region 'disabled nil)

(load-modules '("visual" "lang"))

(setq-default display-line-numbers-width 2)

(setq confirm-kill-processes nil)
(setq fill-column 80)


