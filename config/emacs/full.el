;; -*- lexical-binding: t -*-

;; remove unused UI elements
(tool-bar-mode -1)

;; faces
(defface lighter-yellow nil "Background color lightest yellow.")

;; load theme
(add-to-list 'custom-theme-load-path
       (expand-file-name "themes/" user-emacs-directory))

(use-package autothemer)

(use-package theme-changer
  ;; Set for Toledo, PR, BR
  :config (setq calendar-latitude  -24.735140
                calendar-longitude -53.742062)
  (change-theme 'battery-light 'battery-dark))

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
    (:eval
     (if (buffer-modified-p)
       (propertize " %b " 'face 'lighter-yellow
                   'help-echo (concat "Buffer " (buffer-file-name) " has been modified"))
       (propertize " %b " 'help-echo (concat "Buffer " (buffer-file-name) " has been modified"))))
    (:eval
     (when buffer-read-only
       (propertize " <ro>" 'help-echo "Buffer is marked as read-only")))
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
(set-fontset-font t 'unicode-bmp "FontAwesome")
(set-fontset-font t 'han      "Noto Sans Mono CJK SC")
(set-fontset-font t 'kana     "Noto Sans Mono CJK JP")
(set-fontset-font t 'hangul   "Noto Sans Mono CJK KR")
(set-fontset-font t 'cjk-misc "Noto Sans Mono CJK KR")
(let ((chars
       (if (version< emacs-version "28.1")
           '(#x1f300 . #x1fad0)
           'emoji)))
  (set-fontset-font t chars "FontAwesome"))

;;; else
(use-package display-line-numbers
  :hook ((c-mode c++-mode) . display-line-numbers-mode)
  :config (setq-default display-line-numbers-width 2))

(setq prettify-symbols-unprettify-at-point 'right-edge)

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

;;; external packages
(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package vertico
  :init (vertico-mode)
  :config (setq vertico-count 5))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-clang-use-compile-flags-txt t))

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
(use-package org
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :config
  (setq org-directory "~/doc/note"
        org-adapt-indentation nil
        org-src-preserve-indentation t
        ;; create new frame when editting source block
                                        ; org-src-window-setup 'other-frame
        org-startup-with-inline-images t
        org-indent-indentation-per-level 1
        org-hide-emphasis-markers nil
        org-fontify-whole-block-delimiter-line t
        org-startup-folded nil)
  ;; latex FIXME
  (setq org-startup-with-latex-preview t
        org-latex-inputenc-alist '(("utf8" . "utf8x"))
        org-preview-latex-image-directory (expand-file-name "org-latex" (xdg-cache-home))
                                        ; org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
        )
  ;; babel
  (setq org-confirm-babel-evaluate t)
  (require 'org-tempo)
  )

(use-package olivetti
  :hook (org-mode . olivetti-mode))

(use-package org-roam
  :after org
  :bind (("C-c o r j" . org-roam-dailies-find-today)
         ("C-c o r f" . org-roam-node-find)
         ("C-c o r i" . org-roam-node-insert))
  :custom
  (org-roam-directory org-directory)
  (org-roam-dailies-directory "journal/")
  (org-roam-file-exclude-regexp '("\\.stfolder" "\\.stignore" "\\.stversions" "data/"))
  (org-roam-capture-templates
   '(("r" "random" plain "%?"
      :target (file+head "random/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "persona" plain "%?"
      :target (file+head "persona/${slug}.org" "#+title: ${title}\n#+filetags: :persona:\n")
      :unnarrowed t)
     ("m" "music")
     ("ml" "list" plain "%?"
      :target (file+head "music/list/${slug}.org" "#+title: ${title}\n#+filetags: :music:list:\n")
      :unnarrowed t)
     ("mt" "track" plain "%?"
      :target (file+head "music/track/${slug}.org" "#+title: ${title}\n#+filetags: :music:track:\n")
      :unnarrowed t)
     ("f" "film")
     ("fa" "anime" plain "%?"
      :target (file+head "film/anime/${slug}.org" "#+title: ${title}\n#+filetags: :film:anime:\n")
      :unnarrowed t)
     ("fc" "Cartoon" plain "%?"
      :target (file+head "film/cartoon/${slug}.org" "#+title: ${title}\n#+filetags: :film:cartoon:\n")
      :unnarrowed t)
     ("fm" "movie" plain "%?"
      :target (file+head "film/movie/${slug}.org" "#+title: ${title}\n#+filetags: :film:movie:\n")
      :unnarrowed t)
     ("l" "literature")
     ("lb" "book" plain "%?"
      :target (file+head "literature/book/${slug}.org" "#+title: ${title}\n#+filetags: :literature:book:\n")
      :unnarrowed t)
     ("lm" "manga" plain "%?"
      :target (file+head "literature/manga/${slug}.org" "#+title: ${title}\n#+filetags: :literature:manga:\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("t" "Daily journal" entry "* %?"
      :target (file+head "%<%d-%m-%Y->.org" "#+title: %<%d-%m-%Y>\n#+filetags: :journal:\n"))
     ("w" "Weekly journal" plain "* %?"
      :target (file+head "%<%Y-W%U>.org" "#+title: %<%U>th week of %<%Y>\n#+filetags: :journal:\n")))))

;; auto generate inline latex images
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; automatically tangle blocks when saving buffer
(use-package org-auto-tangle
  :disabled
  :hook (org-mode . org-auto-tangle-mode))

;; copy link on point
(use-package org-cliplink
  :after org
  :bind ("C-c o l c" . org-cliplink))

;; spell checker
(use-package ispell
  :disabled
  :straight nil
  :hook (org-mode . flyspell-mode)
  :config
  ;; csv of dictionaries
  (setq ispell-dictionary
        (c-concat-separated
         '("en_US"
           ; "pt_BR"
           )
         ","))
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary))

;; eshell
(use-package esh-mode
  :straight nil
  :config (setq eshell-directory-name
                (expand-file-name "emacs/eshell/" (xdg-data-home))))

;;; indentation
(defun yeet/disable-tabs () (setq indent-tabs-mode nil))
(defun yeet/enable-tabs  () (setq indent-tabs-mode t  ))

(add-hook       'lisp-mode-hook #'yeet/disable-tabs)
(add-hook 'emacs-lisp-mode-hook #'yeet/disable-tabs)

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

(use-package     bnf-mode :commands bnf-mode)
(use-package     csv-mode :commands csv-mode)
(use-package haskell-mode :commands haskell-mode)
(use-package  tuareg-mode :commands tuareg-mode)
(use-package    yaml-mode :commands yaml-mode)

(use-package web-mode
  :hook (web-mode . (lambda ()
                      (add-to-list 'electric-pair-pairs '(?\< . ?\>))))
  :commands web-mode)

(use-package cc-mode
  :straight nil
  :config (setq c-label-minimum-indentation 0))

(use-package js
  :straight nil
  :config (setq js-indent-level 2))

(use-package julia-mode
  :commands julia-mode
  :config (setq julia-indent-offset 2))

(use-package lua-mode
  :commands lua-mode
  :config (setq lua-indent-level 2))

(use-package markdown-mode
  :commands markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package nix-mode
  :mode "\\.nix\\'"
  :commands nix-mode)

(use-package nushell-mode
  :straight (nushell-mode
             :type git
             :host github
             :repo "azzamsa/emacs-nushell")
  :mode "\\.nu\\'"
  :commands nushell-mode
  :config
  (setq nushell-indent-offset 2))

(use-package prolog
  :commands prolog-mode
  :mode ("\\.pl\\'" . prolog-mode))

(use-package python
  :straight nil
  :config (setq python-indent-offset 2))

(use-package zig-mode
  :commands zig-mode
  :config (setq zig-indent-offset 2))

(use-package rust-mode
  :commands rust-mode
  :config (setq rust-indent-offset 2))

(use-package rustic
  :disabled
  :after rust-mode
  :config (setq rustic-lsp-client nil))

(use-package lisp-mode
  :straight (:type built-in))

(use-package clojure-mode
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
  :commands sly
  :config (setq inferior-lisp-program "/bin/sbcl"))

;; C/C++
(setq compile-command "make")

(use-package eglot
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (when (not (locate-file "clangd" exec-path))
    (warn "`clangd' not in `exec-path'. Emacs won't be able to connect to C/C++ LSP server."))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd" "--enable-config"))))

(defun yeet/cmake-prepare (generate-compile-commands is-release)
  (interactive
   (list (yes-or-no-p "Should generate compile_commands.json? ")
         (yes-or-no-p "Set build type to Release (otherwise Debug)? ")))
  (let ((default-directory (project-root (project-current t)))
        (compile-command
         (concat "cmake -S . -B build"
                 " -D CMAKE_BUILD_TYPE=" (if is-release "Release" "Debug")
                 (if generate-compile-commands " -D CMAKE_EXPORT_COMPILE_COMMANDS=1" nil))))
    (call-interactively 'compile)))

(defun yeet/cmake-build ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compile-command "cmake --build build"))
    (call-interactively 'compile)))

(use-package clang-format
  :hook ((c-mode c++-mode) . (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))
  :config
  (setq clang-format-style "file"
        ;; if no `.clang-format' is found, don't do anything
        clang-format-fallback-style "none"))

;; debugger
(use-package realgud
  :bind ("C-c d" . realgud:gdb))

;; disassembler
(use-package rmsbolt
  :commands rmsbolt)

(use-package yasnippet-snippets)
(use-package yasnippet
  :after yasnippet-snippets
  :hook ((c-mode c++-mode) . yas-minor-mode)
  :config (yas-reload-all))

;; other
(setq-default apropos-do-all t)

;; pop-ups should create a new frame, instead of splitting a window
;; use the system's window manager for managing buffers
;; May not work with all modes
;(setq-default pop-up-frames t
;              pop-up-windows nil)

(setq-default x-select-enable-clipboard t
              x-select-enable-primary nil)

(setq visible-bell nil
      ring-bell-function
      #'(lambda ()
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil #'invert-face 'mode-line)))

(use-package openwith
  :config
  (setq openwith-associations
        `((,(rx ".pdf")                                   "xdg-open"  (file))
          (,(rx ".mp3")                                   "deadbeef" (file))
          (,(rx (or ".mpeg" ".avi" ".wmv" ".mp4" ".mkv")) "mpv"      (file))
          (,(rx (or ".jpg" ".jpeg" ".webp" ".gif"))       "imv"      (file))))
  :init (openwith-mode t))

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(global-dash-fontify-mode)
