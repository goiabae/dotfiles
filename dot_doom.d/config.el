;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cole Jackson"
      user-mail-address "colejson@awesomail.web") ;; .web is my domain terminator. Don't think
                                                  ;; there's any regulations on it.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default: (setq doom-theme 'doom-one)
(load-theme 'doom-battery t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq +doom-dashboard-menu-sections
    (append +doom-dashboard-menu-sections
            '(("Go to today's roam note"
               :icon (all-the-icons-octicon "pencil" :face 'doom-dashboard-menu-title)
               :action org-roam-dailies-goto-today))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/note/org")

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/note/roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-graph-executable "/usr/bin/dot")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("j" "journal" entry
      "* daily relatory\n%?\n* task\n* research" ; (file "~/note/template/journal.org")
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;; normally we'd recommend hooking org-roam-ui after org-roam,
    ;; but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)) ; NOTE: currently ~/note/roam

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5)))

(custom-set-faces
  '(org-level-1 ((t (:inherit default :foreground "#ff402b" :height 1.5)))) ; red
  '(org-level-2 ((t (:inherit default :foreground "#fe6e19" :height 1.4)))) ; orange
  '(org-level-3 ((t (:inherit default :foreground "#fabd2f" :height 1.32)))) ; yellow
  '(org-level-4 ((t (:inherit default :foreground "#b5ba25" :height 1.24))))) ; olive

(setq projectile-project-search-path '("~/source/"))

(setq-default fill-column 80) ;; for use with auto-fill-mode

(setq-default auto-fill-function 'do-auto-fill) ; auto word wrap

(after! hl-todo
  (setq hl-todo-keyword-faces
      (append hl-todo-keyword-faces
              '(("SIDEFX"   . "#5da417"))))) ; radioactive side-effects uhhhh

(defun do-not-require-new-line () (set (make-local-variable 'require-final-newline) nil))
(add-hook 'snippet-mode 'do-not-require-new-line) ; disable snippet new-line character

(add-to-list 'load-path "~/source/mach/tools")
(require 'mach-mode)

(add-to-list 'load-path "~/source/emacs-modes")
(require 'ats-two-mode)

(setq lsp-zig-zls-executable "zls")

(setq indent-tabs-mode nil)
(setq-default tab-width 2)

(setq lisp-indent-offset 2)

(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#484848" :background "#252525")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [46 9] [92 9])))

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
(add-hook 'c-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)
(add-hook 'zig-mode-hook 'disable-tabs)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; (OPTIONAL) Shift width for evil-mode users
;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width tab-width)

(defun toggle-tabs ()
  (if (eq indent-tabs-mode nil)
      (setq indent-tabs-mode t)
    (setq indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))

(defun open-terminal-in-workdir ()
  (interactive)
  (let ((workdir (if (projectile-project-root)
                     (projectile-project-root)
                   default-directory)))
    (call-process-shell-command
     (concat "alacritty --working-directory " workdir) nil 0)))

(map! :leader
      :desc "Org babel tangle"
      "n b t" #'org-babel-tangle)

(map! :leader
      :desc "Open terminal in working directory"
      "o t" #'open-terminal-in-workdir)

;; FIXME: debugging doesnt work at all. exit with errcodes 127 and 1.
(setq lsp-clients-clangd-args
      '("-j=3"
        "--background-index"
	"--clang-tidy"
	"--completion-style=detailed"
	"--header-insertion=never"
	"--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(require 'dap-lldb)

(setq frame-title-format
      '(""
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format "%s/" project-name))))
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))))
