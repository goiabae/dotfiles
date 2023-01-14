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

(load "~/.config/emacs/themes/battery.el")
(require 'battery)

(battery-deftheme battery-light "Warm, light and vibrant colour theme"
 ((((class color) (min-colors #xFFFFFF))
   ((class color) (min-colors #xFF)))

  (grey15 "#101010" "#101010")
  (grey14 "#191919" "#191919")
  (grey13 "#262524" "#262524"); fg
  (grey12 "#353231" "#353231")
  (grey11 "#403c3a" "#403c3a")
  (grey10 "#504845" "#504845")
  (grey9  "#635852" "#635852")
  (grey8  "#7b6c61" "#7b6c61")
  (grey7  "#958172" "#958172")
  (grey6  "#a89984" "#a89984")
  (grey5  "#bdae93" "#bdae93")
  (grey4  "#d2c3a5" "#d2c3a5")
  (grey3  "#e7d8b3" "#e7d8b3")
  (grey2  "#f2e4ba" "#f2e4ba")
  (grey1  "#faedc9" "#faedc9"); bg
  (grey0  "#fef5d8" "#fef5d8")

  ;; darker
  (higher-red    "#751518" "#751518")
  (higher-orange "#64d212" "#64d212")
  (higher-yellow "#654715" "#654715")
  (higher-olive  "#48451c" "#48451x")
  (higher-green  "#1f4a1a" "#1f4a1a")
  (higher-cyan   "#2f4f36" "#2f4f36")
  (higher-purple "#603333" "#603333")

  ;; dark
  (high-red      "#b32924" "#b32924")
  (high-orange   "#a93d0a" "#a93d0a")
  (high-yellow   "#b57614" "#b57614")
  (high-olive    "#79740e" "#79740e")
  (high-green    "#207016" "#207016")
  (high-cyan     "#427a4e" "#427a4e")
  (high-purple   "#894949" "#894949")

  ;; normal
  (red           "#e83437" "#e83437")
  (orange        "#d6580e" "#d6580e")
  (yellow        "#d09420" "#d09420")
  (olive         "#98952a" "#98952a")
  (green         "#3f9f32" "#3f9f32")
  (cyan          "#689e69" "#689e69")
  (purple        "#b16262" "#b16262")

  ;; light
  (low-red       "#fc5052" "#fc5052")
  (low-orange    "#fe6e19" "#fe6e19")
  (low-yellow    "#fabd2f" "#fabd2f")
  (low-olive     "#b5ba25" "#b5ba25")
  (low-green     "#53c345" "#53c345")
  (low-cyan      "#8ec07c" "#8ec07c")
  (low-purple    "#d38686" "#d38686")

  ;; lighter
  (lower-red     "#ff7166" "#ff7166")
  (lower-orange  "#ff8c37" "#ff8c37")
  (lower-yellow  "#ffcb52" "#ffcb52")
  (lower-olive   "#cbd044" "#cbd044")
  (lower-green   "#7adf6d" "#7adf6d")
  (lower-cyan    "#a3d491" "#a3d491")
  (lower-purple  "#f7b4b4" "#f7b4b4")

  ;; theme-specific colors
  ;; dark version has different pair
  (fg  grey13)
  (bg  grey1)
  )
 (custom-theme-set-variables
  'battery-light
  `(ansi-color-names-vector
    [,red
     ,olive
     ,orange
     ,purple
     ,yellow
     ,green
     ,cyan])))

(provide 'battery-light)
;;; battery-light-theme.el ends here
