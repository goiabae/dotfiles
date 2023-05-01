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
(push user-emacs-directory load-path)
(push (expand-file-name "~/lib/elisp") load-path)

(setq straight-base-dir
      (expand-file-name "emacs/straight" (xdg-data-home))
      straight-use-package-by-default t
      straight-cache-autoloads t
      straight-check-for-modifications '(check-on-save find-when-checking))

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

(defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
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
     '("'" . repeat)
     '("<escape>" . ignore)

     '("q" . meow-quit)
     '("w" . meow-mark-word)
     '("e" . meow-next-word)
     '("r" . meow-replace)
     '("t" . meow-till)
     '("y" . meow-save) ; this really is meow-yank
     '("u" . meow-undo)
     '("i" . meow-insert)
     '("o" . meow-open-below)
     '("p" . meow-yank)

     '("a" . meow-append)
     '("s" . meow-kill)
     '("d" . meow-delete)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("h" . meow-left)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("l" . meow-right)

     '("z" . meow-pop-selection)
     ; '("x" . TODO)
     '("c" . meow-change)
     '("v" . meow-visit)
     '("b" . meow-back-word)
     '("n" . meow-search)
     '("m" . meow-join)

     '("Q" . meow-goto-line)
     '("W" . meow-mark-symbol)
     '("E" . meow-next-symbol)
     '("R" . meow-swap-grab)
     ; '("T" . TODO)
     '("Y" . meow-sync-grab)
     '("U" . meow-undo-in-selection)
     '("I" . meow-open-above)
     '("O" . meow-to-block)
     ; '("P" . TODO)

     '("A" . meow-append)
     ; '("S" . TODO)
     '("D" . meow-backward-delete)
     '("F" . isearch-forward-regexp)
     '("G" . meow-grab)
     '("H" . meow-left-expand)
     '("J" . meow-next-expand)
     '("K" . meow-prev-expand)
     '("L" . meow-right-expand)

     ; '("Z" . TODO)
     '("X" . meow-goto-line)
     '("C" . meow-block)
     '("V" . meow-line)
     '("B" . meow-back-symbol)
     ; '("N" . TODO)
     ; '("M" . TODO)
     ))

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

;; This assumes that this version of Emacs has the `bind-key' included
;; in the default distribution
(require 'bind-key)

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
 ("C-c f d"   . delete-frame)
 ("C-c f m"   . make-frame)
 ("C-c i d"   . user-insert-date)
 ("C-c s"     . async-shell-command)
 ("C-c c"     . compile)
 ("C-c t l"   . display-line-numbers-mode)
 ("C-c t t"   . toggle-truncate-lines)
 ("C-c b i"   . ibuffer)
 ("C-c p u"   . straight-use-package)
 ("C-c p r"   . straight-get-recipe))

(blink-cursor-mode -1)
(show-paren-mode 1)

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "emacs/" (xdg-data-home))))

(require 'elec-pair)
(electric-indent-mode nil) ;; disable auto indentation
(setq electric-pair-skip-self t
      electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])))
(setq-default electric-indent-inhibit t)
(electric-pair-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

(setq-default
 save-place-forget-unreadable-files t
 delete-by-moving-to-trash t
 require-final-newline t
 next-line-add-newlines nil ;; don't add newlines when scrolling to bottom
 indent-tabs-mode t ;; if true then indent with tabs else use spaces
 message-log-max 1000
 initial-scratch-message nil
 column-number-mode t
 confirm-kill-processes t)

(setq-default truncate-lines nil
              tab-width 2
              fill-column 70)

(setq visible-cursor nil)

;; don't ask for confirmation when changing region case
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package titlecase)

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs tab-mark trailing)
        whitespace-display-mappings '((tab-mark ?\t [?· ?\t] [?\\ ?\t])))
  :init (global-whitespace-mode))

(use-package which-key
  :init (which-key-mode))

(if (or (display-graphic-p) (daemonp))
    (load "full.el"))
