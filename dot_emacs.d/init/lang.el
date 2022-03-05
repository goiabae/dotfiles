(add-to-list 'load-path "~/source/mach/tools")
(require 'mach-mode)

(defun custom-mach-hook ()
	(setq indent-tabs-mode nil)
	(setq tab-width 2))
(add-hook 'mach-mode-hook 'custom-mach-hook)

(add-to-list 'load-path "~/source/emacs-modes")
(require 'ats-mode)
(require 'nushell-mode)

(load "~/source/yaml-mode/yaml-mode.el")

(load "/usr/local/share/koka/v2.3.8/contrib/emacs/koka-mode.el")

(use-package nix-mode
	:straight t
  :mode "\\.nix\\'")

(add-to-list 'load-path "~/source/tuareg")
(require 'tuareg)

(straight-use-package 'web-mode)
(require 'web-mode)

; (insert "deez nutz in yo mouth")
