
(if (file-directory-p "~/source/mach")
		(progn
			(add-to-list 'load-path "~/source/mach/tools")
      (require 'mach-mode)
      (defun custom-mach-hook ()
        (setq indent-tabs-mode nil)
        (setq tab-width 2))
      (add-hook 'mach-mode-hook 'custom-mach-hook)))

(if (file-directory-p "~/source/emacs-modes")
		(progn
			(add-to-list 'load-path "~/source/emacs-modes")
      (require 'ats-mode)
      (require 'nushell-mode)))

(load "~/source/yaml-mode/yaml-mode.el" t)
(load "/usr/local/share/koka/v2.3.8/contrib/emacs/koka-mode.el" t)

(use-package nix-mode
	:straight t
  :mode "\\.nix\\'")

(if (file-directory-p "~/source/tuareg")
    (progn
      (add-to-list 'load-path "~/source/tuareg")
      (require 'tuareg)))

; (straight-use-package 'web-mode)
; (require 'web-mode)

(use-package csv-mode
  :straight t)

; (insert "deez nutz in yo mouth")
