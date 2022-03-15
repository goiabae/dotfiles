;;; battery-theme.el --- Warm, dark and vibrant color scheme for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Guilherme Machado
;;
;; Author: Guilherme Machado <https://github.com/goiabae>
;; Maintainer: Guilherme Machado <TODO>
;; Created: fevereiro 02, 2022
;; Modified: fevereiro 02, 2022
;; Version: 0.0.1
;; Keywords: convenience extensions faces files frames outlines
;; Homepage: https://github.com/goiabae/battery-theme
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;; Depends on autothemer

;;
;;  Description
;;

;;  A warm, dark and vibrant color scheme inspired by the famous
;;  Gruvbox, from Vim.

;;; Code:
(load "~/.emacs.d/themes/autothemer.el")
(require 'autothemer)

(autothemer-deftheme battery "Warm, dark and vibrant colour theme"

  ;; Specify the color classes used by the theme
  ((((class color) (min-colors #xFFFFFF))
    ((class color) (min-colors #xFF)))

   ;; Specify the color palette for each of the classes above.
  (grey0   "#161616" "#161616")
  (grey1   "#202020" "#202020") ; bg
  (grey2   "#252525" "#252525")
	(grey2.5 "#303030" "#303030")
  (grey3   "#3c3836" "#3c3836")
  (grey4   "#504945" "#504945")
  (grey5   "#7c6f64" "#7c6f64")
  (grey6   "#928374" "#928374")
  (grey7   "#a89984" "#a89984")
  (grey8   "#bdae93" "#bdae93")
  (grey9   "#ebdbb2" "#ebdbb2") ; fg
  (grey10  "#fbf1c7" "#fbf1c7")
	 
  (red           "#cc241d" "#cc241d")
  (olive         "#98952a" "#98952a")
  (yellow        "#d09420" "#d09420")
  (orange        "#d6580e" "#d6580e")
  (purple        "#b16262" "#b16262")
  (green         "#3f9f32" "#3f9f32")
  (cyan          "#689e69" "#689e69")
  (bright-red    "#ff4b38" "#ff4b38")
  (bright-olive  "#b5ba25" "#b5ba25")
  (bright-yellow "#fabd2f" "#fabd2f")
  (bright-orange "#fe6e19" "#fe6e19")
  (bright-purple "#d38686" "#d38686")
  (bright-green  "#53c345" "#53c345")
  (bright-cyan   "#8ec07c" "#8ec07c"))

    ;; specifications for Emacs faces.
  ((default             (:background grey0 :foreground grey9))
	 (button              (:underline t :weight 'bold :foreground yellow))
   (cursor              (:background grey9))
   (mode-line           (:background grey4 :foreground grey9 :box nil))
   (mode-line-inactive  (:background grey3 :foreground grey9 :box nil))
   (fringe              (:background grey1))
   (hl-line             (:background grey1))
   (region              (:background grey4)) ;;selection
   (secondary-selection (:background grey5))
   (minibuffer-prompt   (:background grey1 :foreground bright-green :bold t))
   (vertical-border     (:foreground grey3))
   (internal-border     (:background grey3))
   (window-divider      (:foreground grey4))
   (link                (:foreground bright-yellow :underline t))
   (shadow              (:foreground grey4))

	      ;; Basic faces
   (error               (:foreground red :bold t))
   (success             (:foreground bright-olive :bold t))
   (warning             (:foreground bright-yellow :bold t))
   (alert-low-face      (:foreground bright-cyan))
   (trailing-whitespace (:background red))
   (escape-glyph        (:foreground cyan))
   (header-line         (:background grey5 :foreground grey9 :box nil :inherit nil))
   (highlight           (:background grey5 :foreground grey8))
   (homoglyph           (:foreground yellow))
   (match               (:foreground grey0 :background cyan))

	 ;; Font lock (syntax highlighting)
   (font-lock-builtin-face        (:foreground bright-cyan))
   (font-lock-constant-face       (:foreground bright-purple))
   (font-lock-comment-face        (:foreground grey6))
   (font-lock-function-name-face  (:foreground bright-olive))
   (font-lock-keyword-face        (:foreground bright-red))
   (font-lock-string-face         (:foreground bright-olive))
   (font-lock-variable-name-face  (:foreground bright-yellow))
   (font-lock-type-face           (:foreground bright-orange))
   (font-lock-warning-face        (:foreground bright-red :bold t))

   (line-number               (:foreground grey6 :background grey1))
   (line-number-current-line  (:foreground bright-orange :background grey2))

	 ;; company-mode
   (company-scrollbar-bg                 (:background grey2))
   (company-scrollbar-fg                 (:background grey1))
   (company-tooltip                      (:background grey1))
   (company-tooltip-annotation           (:foreground bright-green))
   (company-tooltip-annotation-selection (:inherit 'company-tooltip-annotation))
   (company-tooltip-selection            (:foreground bright-purple :background grey2))
   (company-tooltip-common               (:foreground bright-orange :underline t))
   (company-tooltip-common-selection     (:foreground bright-orange :underline t))
   (company-preview-common               (:foreground grey9))
   (company-preview                      (:background bright-orange))
   (company-preview-search               (:background bright-cyan))
   (company-template-field               (:foreground grey0 :background bright-yellow))
   (company-echo-common                  (:foreground red))

	 ;; org-mode
   (org-level-1  (:foreground bright-red))
   (org-level-2  (:foreground bright-orange))
   (org-level-3  (:foreground bright-yellow))
   (org-level-4  (:foreground bright-olive))
   (org-level-5  (:foreground bright-cyan))
   (org-level-6  (:foreground bright-purple))
   (org-level-7  (:foreground green))
   (org-level-8  (:foreground orange))
	 (org-verbatim (:foreground bright-yellow))
   (org-block            (:background grey2))
   (org-block-begin-line (:foreground grey4))
   (org-block-end-line   (:foreground grey4))
	 (org-table            (:foreground grey8))

	 )

    ;; Forms after the face specifications are evaluated.
    ;; (palette vars can be used, read below for details.)
    (custom-theme-set-variables 'battery
        `(ansi-color-names-vector [,red
                                   ,olive
                                   ,orange
                                   ,purple
                                   ,yellow
                                   ,green
                                   ,cyan])))

(provide 'battery-theme)
;;; battery-theme.el ends here
