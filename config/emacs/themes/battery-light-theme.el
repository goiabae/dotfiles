;;; battery-light-theme.el --- Warm, dark and vibrant color scheme for Emacs -*- lexical-binding: t; -*-
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

;;  A warm, light and vibrant color scheme inspired by the famous
;;  Gruvbox, from Vim.

;;; Code:

(load (expand-file-name "themes/battery.el" user-emacs-directory))
(require 'battery)

(battery-deftheme battery-light "Warm, light and vibrant colour theme"
  ((grey0  neutral15)
   (grey1  neutral14)
   (grey2  neutral13) ; fg
   (grey3  neutral12)
   (grey4  neutral11)
   (grey5  neutral10)
   (grey6  neutral9)
   (grey7  neutral8)
   (grey8  neutral7)
   (grey9  neutral6)
   (grey10 neutral5)
   (grey11 neutral4)
   (grey12 neutral3)
   (grey13 neutral2)
   (grey14 neutral1) ; bg
   (grey15 neutral0))

 ((higher-red    darker-red)
  (higher-orange darker-orange)
  (higher-yellow darker-yellow)
  (higher-olive  darker-olive)
  (higher-green  darker-green)
  (higher-cyan   darker-cyan)
  (higher-purple darker-purple)

  (high-red      dark-red)
  (high-orange   dark-orange)
  (high-yellow   dark-yellow)
  (high-olive    dark-olive)
  (high-green    dark-green)
  (high-cyan     dark-cyan)
  (high-purple   dark-purple)

  (low-red       light-red)
  (low-orange    light-orange)
  (low-yellow    light-yellow)
  (low-olive     light-olive)
  (low-green     light-green)
  (low-cyan      light-cyan)
  (low-purple    light-purple)

  (lower-red     lighter-red)
  (lower-orange  lighter-orange)
  (lower-yellow  lighter-yellow)
  (lower-olive   lighter-olive)
  (lower-green   lighter-green)
  (lower-cyan    lighter-cyan)
  (lower-purple  lighter-purple)

  (fg  grey13)
  (bg  grey1)))

(provide 'battery-light)
;;; battery-light-theme.el ends here
