;;; battery-dark.el --- Warm, dark and vibrant color scheme for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Guilherme Machado
;;
;; Author: Guilherme Machado <https://github.com/goiabae>
;; Maintainer: Guilherme Machado <TODO>
;; Created: fevereiro 02, 2022
;; Version: 0.0.1
;; Keywords: convenience extensions faces files frames outlines
;; Homepage: https://github.com/goiabae/battery-theme
;; Package-Requires: ((autothemer "0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;;  A warm, dark and vibrant color scheme inspired by the famous
;;  Gruvbox, from Vim.

;;; Code:

(load (expand-file-name "themes/battery.el" user-emacs-directory))
(require 'battery)

(battery-deftheme battery-dark "Warm, dark and vibrant colour theme"
  ;; Specify the color classes used by the theme
  ((grey0  neutral0) ; bg
   (grey1  neutral1)
   (grey2  neutral2)
   (grey3  neutral3)
   (grey4  neutral4)
   (grey5  neutral5)
   (grey6  neutral6)
   (grey7  neutral7)
   (grey8  neutral8)
   (grey9  neutral9)
   (grey10 neutral10)
   (grey11 neutral11)
   (grey12 neutral12) ; fg
   (grey13 neutral13)
   (grey14 neutral14)
   (grey15 neutral15))

  ((lower-red      darker-red)
   (lower-orange   darker-orange)
   (lower-yellow   darker-yellow)
   (lower-olive    darker-olive)
   (lower-green    darker-green)
   (lower-cyan     darker-cyan)
   (lower-purple   darker-purple)

   (low-red        dark-red)
   (low-orange     dark-orange)
   (low-yellow     dark-yellow)
   (low-olive      dark-olive)
   (low-green      dark-green)
   (low-cyan       dark-cyan)
   (low-purple     dark-purple)

   (high-red       light-red)
   (high-orange    light-orange)
   (high-yellow    light-yellow)
   (high-olive     light-olive)
   (high-green     light-green)
   (high-cyan      light-cyan)
   (high-purple    light-purple)

   (higher-red     lighter-red)
   (higher-orange  lighter-orange)
   (higher-yellow  lighter-yellow)
   (higher-olive   lighter-olive)
   (higher-green   lighter-green)
   (higher-cyan    lighter-cyan)
   (higher-purple  lighter-purple)

   (bg "#000000")
   (fg grey12)))

(provide 'battery-dark)
;;; battery-dark.el ends here
