;;; battery-dark.el --- Warm, dark and vibrant color scheme for Emacs -*- lexical-binding: t; -*-
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

(load "~/config/emacs/themes/battery.el")
(require 'battery)

(battery-deftheme battery-dark "Warm, dark and vibrant colour theme"
  ;; Specify the color classes used by the theme
  ((((class color) (min-colors #xFFFFFF))
    ((class color) (min-colors #xFF)))

   ;; Specify the color palette for each of the classes above.
  (grey0  "#101010" "#101010") ; bg
  (grey1  "#191919" "#191919")
  (grey2  "#262524" "#262524")
  (grey3  "#353231" "#353231")
  (grey4  "#403c3a" "#403c3a")
  (grey5  "#504845" "#504845")
  (grey6  "#635852" "#635852")
  (grey7  "#7b6c61" "#7b6c61")
  (grey8  "#958172" "#958172")
  (grey9  "#a89984" "#a89984")
  (grey10 "#bdae93" "#bdae93")
  (grey11 "#d2c3a5" "#d2c3a5")
  (grey12 "#e7d8b3" "#e7d8b3") ; fg
  (grey13 "#f2e4ba" "#f2e4ba")
  (grey14 "#faedc9" "#faedc9")
  (grey15 "#fef5d8" "#fef5d8")

  (lower-red      "#751518" "#751518")
  (lower-orange   "#642d12" "#642d12")
  (lower-yellow   "#654715" "#654715")
  (lower-olive    "#48451c" "#48451c")
  (lower-green    "#1f4a1a" "#1f4a1a")
  (lower-cyan     "#2f4f36" "#2f4f36")
  (lower-purple   "#603333" "#603333")

  (low-red        "#b32924" "#b32924")
  (low-orange     "#d6580e" "#d6580e")
  (low-yellow     "#d09420" "#d09420")
  (low-olive      "#79740e" "#79740e")
  (low-green      "#3f9f32" "#3f9f32")
  (low-cyan       "#427a4e" "#427a4e")
  (low-purple     "#894949" "#894949")

  (red            "#e83437" "#e83437")
  (orange         "#d6580e" "#d6580e")
  (yellow         "#d09420" "#d09420")
  (olive          "#98952a" "#98952a")
  (green          "#3f9f32" "#3f9f32")
  (cyan           "#689e69" "#689e69")
  (purple         "#b16262" "#b16262")

  (high-red       "#fc5052" "#fc5052")
  (high-orange    "#fe6e19" "#fe6e19")
  (high-yellow    "#fabd2f" "#fabd2f")
  (high-olive     "#b5ba25" "#b5ba25")
  (high-green     "#53c345" "#53c345")
  (high-cyan      "#8ec07c" "#8ec07c")
  (high-purple    "#d38686" "#d38686")

  (higher-red     "#ff7166" "#ff7166")
  (higher-orange  "#ff8c37" "#ff8c37")
  (higher-yellow  "#ffcb52" "#ffcb52")
  (higher-olive   "#cbd044" "#cbd044")
  (higher-green   "#7adf6d" "#7adf6d")
  (higher-cyan    "#a3d492" "#a3d492")
  (higher-purple  "#f7b4b4" "#f7b4b4")

  (bg "#000000")
  (fg grey12)
  )
  ;; specifications for Emacs faces.
  ;; Forms after the face specifications are evaluated.
  ;; (palette vars can be used, read below for details.)
  (custom-theme-set-variables
   'battery-dark
   `(ansi-color-names-vector
     [,red
      ,olive
      ,orange
      ,purple
      ,yellow
      ,green
      ,cyan])))

(provide 'battery-dark)
;;; battery-dark.el ends here
