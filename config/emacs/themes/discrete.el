;;; discrete.el --- Warm, dark and vibrant color scheme for Emacs -*- lexical-binding: t; -*-
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

;;  A warm, light and muted color scheme inspired by the famous
;;  Gruvbox, from Vim.

;;; Code:

(load "~/etc/emacs/themes/battery.el")
(require 'battery)

(battery-deftheme discrete "Warm, light and vibrant colour theme"
 ((((class color) (min-colors #xFFFFFF))
   ((class color) (min-colors #xFF)))

  (grey15 "#101010" "#101010")
  (grey14 "#191919" "#191919")
  (grey13 "#262524" "#262524")
  (grey12 "#353231" "#353231"); fg
  (grey11 "#3f3c3b" "#3f3c3b")
  (grey10 "#4d4846" "#4d4846")
  (grey9  "#5e5854" "#5e5854")
  (grey8  "#726861" "#726861")
  (grey7  "#8d7e72" "#8d7e72")
  (grey6  "#a69a89" "#a69a89")
  (grey5  "#bbaf9c" "#bbaf9c")
  (grey4  "#cfc3ac" "#cfc3ac")
  (grey3  "#e4d8bc" "#e4d8bc")
  (grey2  "#efe4c4" "#efe4c4")
  (grey1  "#f8edd1" "#f8edd1"); bg
  (grey0  "#fcf5de" "#fcf5de")

  ;; darker
  (higher-red    "#622b2d" "#622b2d")
  (higher-orange "#543427" "#543427")
  (higher-yellow "#57462c" "#57462c")
  (higher-olive  "#413f29" "#413f29")
  (higher-green  "#2a4227" "#2a4227")
  (higher-cyan   "#37493b" "#37493b")
  (higher-purple "#563c3c" "#563c3c")

  ;; dark
  (high-red    "#8c3537" "#8c3537")
  (high-orange "#8c4e39" "#8c4e39")
  (high-yellow "#9a7547" "#9a7547")
  (high-olive  "#6a6736" "#6a6736")
  (high-green  "#376233" "#376233")
  (high-cyan   "#517057" "#517057")
  (high-purple "#7b5555" "#7b5555")

  ;; normal
  (red    "#a94b49" "#a94b49")
  (orange "#b2694a" "#b2694a")
  (yellow "#b28f58" "#b28f58")
  (olive  "#87864f" "#87864f")
  (green  "#588d52" "#588d52")
  (cyan   "#769476" "#769476")
  (purple "#9f7272" "#9f7272")

  ;; light
  (low-red    "#d77067" "#d77067")
  (low-orange "#d4815c" "#d4815c")
  (low-yellow "#d8b570" "#d8b570")
  (low-olive  "#a2a45c" "#a2a45c")
  (low-green  "#71ae6a" "#71ae6a")
  (low-cyan   "#97b48e" "#97b48e")
  (low-purple "#c19595" "#c19595")

  ;; lighter
  (lower-red    "#df8d87" "#df8d87")
  (lower-orange "#d9976d" "#d9976d")
  (lower-yellow "#e1c383" "#e1c383")
  (lower-olive  "#b8bb72" "#b8bb72")
  (lower-green  "#9dcb97" "#9dcb97")
  (lower-cyan   "#acc8a3" "#acc8a3")
  (lower-purple "#e6c1c1" "#e6c1c1")

  ;; theme-specific colors
  ;; dark version has different pair
	(fg  grey12)
	(bg  grey1)
  )
 (custom-theme-set-variables
  'discrete
  `(ansi-color-names-vector
    [,red
     ,olive
     ,orange
     ,purple
     ,yellow
     ,green
     ,cyan])))

(provide 'discrete)
;;; discrete.el ends here
