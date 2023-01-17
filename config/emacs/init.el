;; -*- lexical-binding: t -*-

(require 'xdg)
(require 'map)

;; Emacs >= 29.0
;; perform compilation ahead-of-time. This ensures that no compilation
;; occurs asynchronously during the actual use of the editor, which
;; could cause slowdowns
(when (fboundp 'native-compile-async)
  (setq native-comp-deferred-compilation nil
        comp-deferred-compilation nil)
  (push
   (expand-file-name "emacs/native-lisp" (xdg-cache-home))
   native-comp-eln-load-path))

;; message after inatialization showing time and amount of garbage
;; collections
(add-hook 'emacs-startup-hook
    (lambda ()
      (message "Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
           (float-time
            (time-subtract after-init-time before-init-time)))
         gcs-done)))

;;; text encoding
(prefer-coding-system          'utf-8)
(set-default-coding-systems    'utf-8-unix)
(set-selection-coding-system   'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system    'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-clipboard-coding-system   'utf-8)

(when (eq system-type 'windows-nt)
  (set-w32-system-coding-system 'utf-8))

(set-language-environment "UTF-8")
;; override system locale
; (set-locale-environment "en_US.UTF-8")

;; dont't show these warnings during byte compileation
(setq warning-suppress-types '((comp))
      byte-compile-warnings
      '(not obsolete interactive-only lexical docstrings))

(setq user-emacs-directory (expand-file-name "emacs" (xdg-config-home)))

(setq straight-base-dir
      (expand-file-name "emacs/straight" (xdg-data-home)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
         (concat straight-base-dir "/straight/repos/straight.el/bootstrap.el")))
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
(setq straight-use-package-by-default t)

(defmacro defcommand (name args &optional interactive docstring &rest body)
  "Define an interactive command. NAME is the command name, ARGS are
the command arguments. INTERACTIVE is the string passed to
`interactive' and DOCSTRING is the function docstring. BODY is the
functions' body."
  `(defun ,name ,args
     ,docstring
     ,(if interactive
    `(interactive ,interactive)
  '(interactive))
     ,@body))

(bind-keys
 ("C-c w s b" . split-window-below)
 ("C-c w s r" . split-window-right)
 ("C-c w d"   . delete-window)
 ("C-c w h"   . windmove-left)
 ("C-c w j"   . windmove-down)
 ("C-c w k"   . windmove-up)
 ("C-c w l"   . windmove-right)
 ("C--"       . text-scale-decrease)
 ("C-="       . text-scale-increase)
 ("C-c r"     . yeet/reload-config)
 ("C-c f d"   . delete-frame)
 ("C-c f m"   . make-frame)
 ("C-c i d"   . user-insert-date)
 ("C-c s"     . async-shell-command)
 ("C-c c"     . compile)
 ("C-c t l"   . display-line-numbers-mode)
 ("C-c t t"   . toggle-truncate-lines)
 ("C-c b i"   . ibuffer)
 ("C-c o j"   . yeet/open-weekly-journal)
 ("C-c p u"   . straight-use-package)
 ("C-c p r"   . straight-get-recipe))

(defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ; '("w" . "C-c w")
     '("h" . "C-h")
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("$" . move-end-of-line)
     '("^" . move-beginning-of-line)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-append)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . isearch-forward-regexp)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-open-below)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("V" . meow-line)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save) ; this really is meow-yank
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup)
  (setq meow-use-clipboard t)
  (meow-global-mode 1)
  (meow-normal-define-key
    ; move point to next/previous parentheses
    '(")" . forward-list)
    '("(" . backward-list)
    '("/" . isearch-forward-regexp)))

(use-package which-key
  :init (which-key-mode))

(setq temporary-file-directory
      (expand-file-name "emacs/" (xdg-runtime-dir)))

(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory))

(setq auto-save-default nil
      delete-auto-save-files t
      auto-save-list-file-prefix
      (expand-file-name "auto-save" temporary-file-directory)
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(auto-save-mode -1)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" temporary-file-directory))
        (,tramp-file-name-regexp . nil))
      make-backup-files t)

(setq create-lockfiles nil)

;; keep custom writting to `init.el'
(setq-default custom-file
        (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(use-package no-littering)

;; remove unused UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; load theme
(add-to-list 'custom-theme-load-path
       (expand-file-name "themes/" user-emacs-directory))

(use-package autothemer)
(load-theme 'battery-dark)

(use-package theme-changer
  :disabled
  :config (setq calendar-latitude  -23.61
                calendar-longitude -46.65)
  :init (change-theme 'battery-light 'battery-dark))

(show-paren-mode 1)

(require 'elec-pair)
(electric-indent-mode nil) ;; disable auto indentation
(setq electric-pair-skip-self t
      electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])))
(setq-default electric-indent-inhibit t)
(electric-pair-mode t)

(setq mouse-autoselect-window t)

(setq scroll-step 1
      scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scrolll-down-aggressively 0.01
      hscroll-step 1
      hscroll-margin 1
      auto-window-vscroll nil ;; reduce cursor lag
      )

;; title and mode line
(setq frame-title-format
      '(buffer-line-name "Emacs %b (%f)" "Emacs %b")
      frame-resize-pixelwise t)

(map-put default-frame-alist 'width  40)
(map-put default-frame-alist 'height 22)

;; git
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:added-sign "+"
        git-gutter:modified-sign "~"
        git-gutter:deleted-sign "-"
        git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :defer t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added    [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted  [128 192 224 240] nil nil 'bottom))

(setq-default mode-line-format
  '((:eval (meow-indicator))
    "%b"
    (:eval
     (when (buffer-modified-p)
       (propertize "M" 'help-echo "Buffer has been modified")))
    (:eval
     (when buffer-read-only
       (propertize "L" 'help-echo "Buffer is marked as read-only")))
    " "
    mode-line-position
    " "
    mode-line-misc-info
    " "
    (:eval
     (propertize " " 'display
           `((space
        :align-to (-
             (+ right right-fringe right-margin)
             ,(+ 3 (string-width
              (if (listp mode-name)
                  (car mode-name)
                  mode-name))))))))))

;;; fonts
(set-face-attribute 'default nil
 :family "SauceCodePro Nerd Font Mono" :height 100 :inherit 'default)

(set-face-attribute 'fixed-pitch nil
 :family "SauceCodePro Nerd Font Mono" :height 100 :inherit 'default)

(set-face-attribute 'variable-pitch nil
 :family "NotoSans Display Nerd Font" :height 105)

;; set font for character sets from languages of East Asia
(set-fontset-font t 'unicode-bmp   (font-spec :family "FontAwesome"))
(set-fontset-font t 'han      "Noto Sans Mono CJK SC")
(set-fontset-font t 'kana     "Noto Sans Mono CJK JP")
(set-fontset-font t 'hangul   "Noto Sans Mono CJK KR")
(set-fontset-font t 'cjk-misc "Noto Sans Mono CJK KR")
(set-fontset-font t
                  (if (version< emacs-version "28.1")
                      '(#x1f300 . #x1fad0)
                      'emoji)
                  (font-spec :family "FontAwesome"))

;;; else
(use-package whitespace
  :config
  (setq whitespace-style '(face tabs tab-mark trailing)
  whitespace-display-mappings '((tab-mark 9 [183 9] [92 9])))
  :init (global-whitespace-mode))

(use-package display-line-numbers
  :hook ((c-mode c++-mode) . display-line-numbers-mode)
  :config (setq-default display-line-numbers-width 2))

(setq prettify-symbols-unprettify-at-point 'right-edge)

;;; external packages
(use-package hide-mode-line
  :defer t
  :commands hide-mode-line-mode)

(use-package olivetti)

(use-package vertico
  :init (vertico-mode)
  :config (setq vertico-count 5))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-selection-wrap-around t
        company-minimum-prefix-length 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrrides
        '((file (styles partial-completion)))))

(use-package dired
  :straight nil
  :hook ((dired-mode . auto-revert-mode)
         (dired-mode . dired-hide-details-mode)))

;;; org-mode
(require 'org)

(setq org-directory "~/doc/note"
      org-adapt-indentation nil
      org-src-preserve-indentation t
      ;; create new frame when editting source block
      org-src-window-setup 'other-frame
      org-startup-with-inline-images t
      org-indent-indentation-per-level 1
      org-hide-emphasis-markers nil
      org-fontify-whole-block-delimiter-line t)

;; latex
(setq org-startup-with-latex-preview t
      org-latex-inputenc-alist '(("utf8" . "utf8x"))
      org-preview-latex-image-directory
        (expand-file-name "org-latex" (xdg-cache-home))
      org-format-latex-options
        (plist-put org-format-latex-options :scale 1.4))

;; babel
(setq org-confirm-babel-evaluate t)

;; TODO maybe use `org-roam' for this (?)
(defvar yeet/org-weekly-journal-directory
  (expand-file-name "journal/weekly/" org-directory))

(defcommand yeet/open-weekly-journal ()
  (find-file
   (expand-file-name
    (format-time-string "%U-%Y\.org")
    yeet/org-weekly-journal-directory)))

(require 'org-tempo)

(add-hook 'org-mode-hook #'prettify-symbols-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook
          #'(lambda ()
              (setq left-margin-width 2.0
                    fill-column 60)))

;; spell checker
(use-package ispell
  :straight nil
  :hook (org-mode . flyspell-mode)
  :config
  ;; csv of dictionaries
  (setq ispell-dictionary "en_US,pt_BR"))

;; auto generate inline latex images
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; automatically tangle blocks when saving buffer
(use-package org-auto-tangle
  :disabled
  :hook (org-mode . org-auto-tangle-mode))

;; copy link on point
(use-package org-cliplink
  :commands org-cliplink
  :bind ("C-c o l c" . org-cliplink))

;; eshell
(use-package esh-mode
  :straight nil
  :config
  (setq eshell-directory-name
        (expand-file-name "emacs/eshell/" (xdg-data-home))))



(setq gdb-show-main t)
(setq gdb-many-windows t)

;;; indentation
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  () (setq indent-tabs-mode t  ))

(add-hook       'lisp-mode-hook #'disable-tabs)
(add-hook 'emacs-lisp-mode-hook #'disable-tabs)

;; FIXME: temporary solution
(defvar yeet/colors
  '((purple . "#b5ba25")
    (red    . "#fc3f42")
    (olive  . "#d38686")
    (yellow . "#fabd2f")))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(alist-get 'purple yeet/colors))
          ("FIXME" . ,(alist-get 'red    yeet/colors))
          ("NOTE"  . ,(alist-get 'olive  yeet/colors))
          ("WARN"  . ,(alist-get 'yellow yeet/colors)))))

(use-package     bnf-mode :defer t :commands bnf-mode)
(use-package     csv-mode :defer t :commands csv-mode)
(use-package haskell-mode :defer t :commands haskell-mode)
(use-package  tuareg-mode :defer t :commands tuareg-mode)
(use-package     web-mode :defer t :commands web-mode)
(use-package    yaml-mode :defer t :commands yaml-mode)

(use-package cc-mode
  :straight nil
  :config (setq c-label-minimum-indentation 0))

(use-package js
  :straight nil
  :config (setq js-indent-level 2))

(use-package julia-mode
  :defer t
  :commands julia-mode
  :config (setq julia-indent-offset 2))

(use-package lua-mode
  :defer t
  :commands lua-mode
  :config (setq lua-indent-level 2))

(use-package markdown-mode
  :defer t
  :commands markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :commands nix-mode)

(use-package nushell-mode
  :disabled
  :straight
  (nushell-mode :type git :host github :repo "azzamsa/nushell-mode")
  :config (setq nushell-indent-offset 2))

(use-package prolog
  :defer t
  :commands prolog-mode
  :mode ("\\.pl\\'" . prolog-mode))

(use-package python
  :straight nil
  :config (setq python-indent-offset 2))

(use-package zig-mode
  :defer t
  :commands zig-mode
  :config (setq zig-indent-offset 2))

(use-package rust-mode
  :defer t
  :commands rust-mode
  :config (setq rust-indent-offset 2))

(use-package rustic
  :disabled
  :config
  (setq rustic-lsp-client nil))

(use-package lisp-mode
  :straight (:type built-in))

(use-package clojure-mode
  :defer t
  :commands clojure-mode
  :config
  ;; this makes so that clojure code is always indented relative
  ;; to the indentation of the current root and not to the arguments
  ;; of a function
  (setq clojure-indent-style 'always-indent))

(use-package rainbow-delimiters
  :hook ((lisp-data-mode clojure-mode) . rainbow-delimiters-mode))

;; Common Lisp
(use-package sly
  :defer t
  :commands sly
  :config (setq inferior-lisp-program "/bin/sbcl"))

;;; ide/debugging/programming support
(setq compile-command "make ")

;; C/C++
(use-package lsp-mode
  :defer t
  ; :requires lsp-clangd
  :hook ((c-mode c++-mode) . lsp)
  :config
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable nil))

(use-package lsp-clangd
  :disabled
  :defer t
  :config
  (add-to-list 'lsp-clients-clangd-library-directories "/home/goiabae/lib/c"))

(use-package dap-mode
  :defer t
  :requires (dap-lldb dap-gdb-lldb)
  :init
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (dap-auto-configure-mode 1)
  (dap-ui-controls-mode 1)
  :bind (:map dap-mode-map
         ([f5] . dap-debug)
         ("M-d i" . dap-step-in)
         ("M-d o" . dap-step-out)
         ("M-d n" . dap-next)
         ("M-d g" . dap-continue)
         ("M-d t" . dap-breakpoint-toggle))
  :config (setq dap-lldb-debug-program "/usr/bin/lldb-vscode"))

(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state) (save-some-buffers t))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

(setq-default
 save-place-forget-unreadable-files t
 delete-by-moving-to-trash t
 indent-tabs-mode t ;; if true then indent with tabs else use spaces
 require-final-newline t
 next-line-add-newlines nil ;; don't add newlines when scrolling to bottom
 apropos-do-all t
 message-log-max 1000
 initial-scratch-message nil
 column-number-mode t
 confirm-kill-processes t)

;; pop-ups should create a new frame, instead of splitting a window
;; use the system's window manager for managing buffers
;; May not work with all modes
(setq-default pop-up-frames t
              pop-up-windows nil)

(setq-default x-select-enable-clipboard t
              x-select-enable-primary nil)

(setq-default truncate-lines nil
              tab-width 2
              fill-column 70)

(setq visible-cursor nil
      visible-bell nil
      ring-bell-function
      #'(lambda ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line)))

;; don't ask for confirmation when changing region case
(put   'upcase-region 'disabled nil)

;;; utility functions and commands
(defcommand yeet/reload-config ()
  (load (expand-file-name "init.el" user-emacs-directory)))

(defcommand yeet/insert-date ()
  (insert (format-time-string "%d/%m/%Y")))

(defun yeet/pipe-in (a)
  (let ((b (create-file-buffer "*stdin*")))
    (switch-to-buffer b)
    (insert-file-contents a)
    (delete-file a)
    (pipe-mode)))

(defcommand yeet/pipe-out ()
  (with-current-buffer (get-buffer "*stdin*")
    (write-file "/dev/stdout")))

(use-package openwith
  :config
  (setq openwith-associations
        '(("\\.pdf\\'"                                   "zathura"  (file))
          ("\\.mp3\\'"                                   "deadbeef" (file))
          ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|mkv\\)\\'" "mpv"      (file))
          ("\\.\\(?:jp?g\\|png\\|webp\\|gif\\)\\'"       "imv"      (file))))
  :init (openwith-mode t))

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(global-dash-fontify-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
