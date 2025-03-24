;;; bison-ts-mode --- tree-sitter support for Bison -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author   : Augustin Chéneau <btuin@mailo.com>
;; Keywords : bison yacc languages tree-sitter

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a mode based on tree-sitter for Bison and Yacc files, tools to
;; generate parsers.  The grammar used is available here:
;; https://gitlab.com/btuin2/tree-sitter-bison

;;; Code:

(require 'treesit)
(require 'c-ts-common)
(require 'c-ts-mode) ; For ‘c-ts-mode--get-indent-style’.

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-string "treesit.c")
(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-capture "treesit.c")
(declare-function treesit-parser-add-notifier "treesit.c")
(declare-function treesit-parser-buffer "treesit.c")
(declare-function treesit-parser-list "treesit.c")


(defgroup bison-ts nil
  "Support for Bison and Yacc."
  :group 'languages)

(defcustom bison-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `bison-ts-mode'.
It has no effect in the epilogue part of the file."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'bison)

(defcustom bison-ts-mode-autodetect-language nil
  "Search for a %language directive in the file at initialization.
Changing the value of this directive in the file requires to
reload the mode to be effective. If
`bison-ts-mode-buffer-language' is set by a file-local variable,
the auto-detection is not run."
  :version "30.1"
  :type 'boolean
  :safe 'boolean
  :group 'bison)

(defvar-local bison-ts-mode-embedded-language 'cpp
  "Embedded language in Bison buffer.
Supported values are `c' and `cpp'.")
;;;###autoload
(put 'bison-ts-mode-embedded-language 'safe-local-variable 'symbolp)


(defun bison-ts-mode--merge-feature-lists (l1 l2)
  "Merge the lists of lists L1 and L2.
The first sublist of L1 is merged with the first sublist of L2 and so on.
L1 and L2 don't need to have the same size."
  (let ((res ()))
    (while (or l1 l2)
      (setq res (push (seq-uniq (append (car l1) (car l2)) 'eq) res))
      (setq l1 (cdr l1) l2 (cdr l2)))
    (nreverse res)))

(defun bison-ts-mode--find-language-in-buffer (&optional buffer)
  "Find and return the language set by the Bison directive %language.
If BUFFER is set, search in this buffer, otherwise search in the current
buffer."
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (when
	  (re-search-forward
	   (rx
	    bol (0+ blank) "%language" (0+ blank) "\"" (group (1+ (in alpha "+"))) "\"")
	   nil
	   t)))
    (substring-no-properties (match-string 1))))


(defun bison-ts-mode--detect-language (&optional buffer)
  "Dectect the embedded language in a Bison buffer.
Known languages are C, C++, D, but D is not supported as there is
no support for tree-sitter D in Emacs yet.
If BUFFER is set, search in this buffer, otherwise search in the current
buffer."
  (if-let ((str (bison-ts-mode--find-language-in-buffer buffer)))
      (pcase-exhaustive (downcase str)
        ("c" 'c)
        ("c++" 'cpp)
        ("d" (message "D language not yet supported") nil)
	(_ (message "%%language specification \"%s\" is invalid, defaulting to C" str) 'c))))


(defun bison-ts-mode--language-at-point-function (position)
  "Return the language at POSITION."
  (let ((node (treesit-node-at position 'bison)))
    (if (equal (treesit-node-type node) "embedded_code")
        bison-ts-mode-embedded-language
      'bison)))

(defun bison-ts-mode--font-lock-settings (language)
  "Return the font-lock settings for Bison.
LANGUAGE should be set to \\='bison."
  (treesit-font-lock-rules
   :language language
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language language
   :feature 'declaration
   '((declaration_name) @font-lock-keyword-face)

   :language language
   :feature 'type
   '((type) @font-lock-type-face)

   :language language
   :feature 'variable
   '((grammar_rule_identifier) @font-lock-variable-use-face)

   :language language
   :feature 'grammar-declaration
   '((grammar_rule (grammar_rule_declaration)
                   @font-lock-variable-use-face))

   :language language
   :feature 'string
   :override t
   '((string) @font-lock-string-face)

   :language language
   :feature 'literal
   :override t
   '((char_literal) @font-lock-keyword-face
     (number_literal) @font-lock-number-face)

   :language language
   :feature 'directive-grammar-rule
   :override t
   '((grammar_rule (directive) @font-lock-keyword-face))

   :language language
   :feature 'operator
   :override t
   '(["|"] @font-lock-operator-face)

   :language language
   :feature 'delimiter
   :override t
   '([";"] @font-lock-delimiter-face)))

(defvar c-ts-mode--feature-list
              '(( comment definition)
                ( keyword preprocessor string type)
                ( assignment constant escape-sequence label literal)
                ( bracket delimiter error function operator property variable)))

(defvar bison-ts-mode--font-lock-feature-list
  '(( comment declaration grammar-declaration)
    ( type string directive-grammar-rule)
    ( literal)
    ( variable operator delimiter)))


(defun bison-ts-mode--bison-matcher-action (root-name)
  "Treesit matcher to check if NODE at BOL is located in an action node.
ROOT-NAME is the highest-level node of the embedded language."
  (lambda (node _parent bol &rest _)
    (if (equal (treesit-node-type (treesit-node-parent node)) root-name)
        (let ((bison-node (treesit-node-at bol 'bison)))
          (equal
           (treesit-node-type
            (treesit-node-parent (treesit-node-parent bison-node)))
	   "action")))))

(defun bison-ts-mode--bison-matcher-not-epilogue (root-name)
  "Treesit matcher to check if NODE at BOL is not located in the epilogue.
ROOT-NAME is the highest-level node of the embedded language."
  (lambda (node _parent bol &rest _)
    (if (equal (treesit-node-type (treesit-node-parent node)) root-name)
        (let ((bison-node (treesit-node-at bol 'bison)))
          (not (equal (treesit-node-type (treesit-node-parent bison-node)) "epilogue"))))))


(defun bison-ts-mode--bison-parent (_node _parent bol &rest _)
  "Get the parent of the bison node at BOL."
  (treesit-node-start (treesit-node-parent (treesit-node-at bol 'bison))))


(defun bison-ts-mode--indent-rules ()
  "Indent rules supported by `bison-ts-mode'."
  (let*
      ((common
        `(((node-is "^declaration$")
           column-0 0)
          ((and (parent-is "^declaration$")
                (not (node-is "^code_block$")))
           column-0 2)
          ((and (parent-is "comment") c-ts-common-looking-at-star)
           c-ts-common-comment-start-after-first-star -1)
          (c-ts-common-comment-2nd-line-matcher
           c-ts-common-comment-2nd-line-anchor
           1)
          ((parent-is "comment") prev-adaptive-prefix 0)

          ;; Opening and closing brackets "{}" of declarations
          ((and (parent-is "^declaration$")
                (node-is "^code_block$"))
           column-0 0)
          ((and (n-p-gp "}" "" "^declaration$"))
           column-0 0)
          ((parent-is "^declaration$") parent 2)
          ((node-is "^grammar_rule$") column-0 0)
          ((and
            (parent-is "^grammar_rule$")
            (node-is ";"))
           column-0 bison-ts-mode-indent-offset)
          ((and (parent-is "^grammar_rule$")
                (node-is "|"))
           column-0 bison-ts-mode-indent-offset)
          ((and (parent-is "^grammar_rule$")
                (not (node-is "^grammar_rule_declaration$"))
                (not (node-is "^action$")))
           column-0 ,(+ bison-ts-mode-indent-offset 2))
          ((or
            (node-is "^action$")
            (node-is "^}$"))
           column-0 12)
          ;; Set '%%' at the beginning of the line
          ((or
            (and (parent-is "^grammar_rules_section$")
                 (node-is "%%"))
            (node-is "^grammar_rules_section$"))
           column-0 0)
          (no-node parent-bol 0))))
    `((bison . ,common)
      ;; Import and override embedded languages rules to add an offset
      ,(pcase bison-ts-mode-embedded-language
         ('c `(c
               ((bison-ts-mode--bison-matcher-action "translation_unit")
                bison-ts-mode--bison-parent ,bison-ts-mode-indent-offset)
               ((bison-ts-mode--bison-matcher-not-epilogue "translation_unit")
                column-0 ,bison-ts-mode-indent-offset)
               ,@(alist-get 'c (c-ts-mode--get-indent-style 'c))))
         ('cpp `(cpp
                 ((bison-ts-mode--bison-matcher-action "translation_unit")
                  bison-ts-mode--bison-parent ,bison-ts-mode-indent-offset)
                 ((bison-ts-mode--bison-matcher-not-epilogue "translation_unit")
                  parent-0 ,bison-ts-mode-indent-offset)
                 ,@(alist-get 'cpp (c-ts-mode--get-indent-style 'cpp))))))))


(define-derived-mode bison-ts-mode prog-mode "Bison"
  "A major-mode for Bison based on tree-sitter."
  (when (treesit-ready-p 'bison)
    (when (not bison-ts-mode-embedded-language)
      (setq bison-ts-mode-embedded-language (bison-ts-mode--detect-language)))

    ;; Require only if needed, to avoid warnings if a grammar is not
    ;; installed but not used.
    (pcase bison-ts-mode-embedded-language
      ('c (require 'c-ts-mode))
      ('cpp (require 'c-ts-mode)))

    (setq-local treesit-font-lock-settings
                (append (bison-ts-mode--font-lock-settings 'bison)
                        (pcase bison-ts-mode-embedded-language
                          ('c (c-ts-mode--font-lock-settings 'c))
                          ('cpp (c-ts-mode--font-lock-settings 'cpp)))))

    (setq-local treesit-font-lock-feature-list
                (if bison-ts-mode-embedded-language
                    (bison-ts-mode--merge-feature-lists
                     bison-ts-mode--font-lock-feature-list
                     (pcase bison-ts-mode-embedded-language
                       ('c c-ts-mode--feature-list)
                       ('cpp c-ts-mode--feature-list)))
                  bison-ts-mode--font-lock-feature-list))

    (setq-local treesit-simple-imenu-settings
                `(("Grammar"
                   "\\`grammar_rule_declaration\\'"
                   nil
                   (lambda (node) (substring-no-properties (treesit-node-text node))))))

    (c-ts-common-comment-setup)

    (setq-local treesit-simple-indent-rules
                (bison-ts-mode--indent-rules))

    (setq-local treesit-language-at-point-function 'bison-ts-mode--language-at-point-function)

    (when bison-ts-mode-embedded-language
      (setq-local treesit-range-settings
                  (treesit-range-rules
                   :embed bison-ts-mode-embedded-language
                   :host 'bison
                   ;; :local t
                   '((embedded_code) @capture))))

    (treesit-major-mode-setup)))

(provide 'bison-ts-mode)

;;; bison-ts-mode.el ends here
