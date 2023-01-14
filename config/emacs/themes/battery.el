(require 'autothemer)

(defmacro battery-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette
    ((default             (:background bg :foreground fg))
     (button              (:underline t :weight 'bold :foreground yellow))
     (cursor              (:background fg))
     (mode-line           (:background grey5 :foreground bg :box (:line-width 1 :color "#f8edd1" :style 'released-button))) ; FIXME hardcoded
     (mode-line-inactive  (:background grey3 :foreground bg :box (:line-width 1 :color "#efe4c4" :style 'released-button))) ; FIXME hardcoded
     (fringe              (:background bg))
     (hl-line             (:background bg))
     (region              (:background grey5)) ;;selection
     (secondary-selection (:background grey6))
     (minibuffer-prompt   (:background bg :foreground high-green :bold t))
     (vertical-border     (:foreground grey7))
     (internal-border     (:background grey7))
     (window-divider      (:foreground grey8))
     (link                (:foreground high-yellow :underline t))
     (shadow              (:foreground grey8))

     ;; Basic faces
     (error               (:foreground red :bold t))
     (success             (:foreground high-olive :bold t))
     (warning             (:foreground high-yellow :bold t))
     (alert-low-face      (:foreground high-cyan))
     (trailing-whitespace (:background red))
     (escape-glyph        (:foreground cyan))
     (header-line         (:background grey7 :foreground grey13 :box nil :inherit nil))
     (highlight           (:background grey4 :foreground fg))
     (homoglyph           (:foreground yellow))
     (match               (:foreground grey0 :background cyan))

     ;; Font lock (syntax highlighting)
     (font-lock-builtin-face        (:foreground high-cyan))
     (font-lock-constant-face       (:foreground high-purple :bold nil))
     (font-lock-comment-face        (:foreground grey8))
     (font-lock-function-name-face  (:foreground high-yellow :bold nil))
     (font-lock-keyword-face        (:foreground high-red))
     (font-lock-string-face         (:foreground high-olive))
     (font-lock-variable-name-face  (:foreground high-yellow))
     (font-lock-type-face           (:foreground high-orange))
     (font-lock-warning-face        (:foreground high-red :bold t))
     (font-lock-preprocessor-face   (:foreground high-cyan))
     (font-lock-negation-char-face  (:foreground high-red))

     (tuareg-font-lock-interactive-directive-face (:foreground high-cyan))
     (tuareg-font-lock-operator-face (:foreground grey11))
     (tuareg-font-lock-governing-face (:foreground high-red))
     (tuareg-font-double-semicolon-face (:foreground high-red))
     (tuareg-font-lock-module-face (:foreground high-yellow))

     (line-number               (:foreground grey10 :background bg))
     (line-number-current-line  (:foreground high-orange :background grey3))

     ;; company-mode
     (company-scrollbar-bg                 (:background grey5))
     (company-scrollbar-fg                 (:background bg))
     (company-tooltip                      (:background bg))
     (company-tooltip-annotation           (:foreground high-green))
     (company-tooltip-annotation-selection (:inherit 'company-tooltip-annotation))
     (company-tooltip-selection            (:foreground high-purple :background grey5))
     (company-tooltip-common               (:foreground high-orange :underline t))
     (company-tooltip-common-selection     (:foreground high-orange :underline t))
     (company-preview-common               (:foreground grey13))
     (company-preview                      (:background high-orange))
     (company-preview-search               (:background high-cyan))
     (company-template-field               (:foreground grey0 :background high-yellow))
     (company-echo-common                  (:foreground red))

     ;; org-mode
     (org-latex-and-related     (:inherit 'fixed-pich :foreground lower-yellow))
     (org-document-info-keyword (:inherit 'fixed-pitch))
     (org-document-title        (:foreground high-purple))
     (org-block-begin-line      (:inherit 'fixed-pitch
                                 :foreground grey6
                                 :background grey0))
     (org-block-end-line        (:inherit 'fixed-pitch
                                 :foreground grey6
                                 :background grey0))
     (org-special-keyword       (:inherit (font-lock-comment-face
                                           'fixed-pitch)))
     (org-property-value        (:inherit 'fixed-pitch))
     (org-document-info         (:foreground orange))
     (org-meta-line             (:inherit (font-lock-comment-face
                                           'fixed-pitch)))
     (org-verbatim (:inherit 'fixed-pitch))
     (org-level-1  (:foreground high-orange :inherit 'fixed-pitch))
     (org-level-2  (:foreground high-red :inherit 'fixed-pitch))
     (org-level-3  (:foreground high-yellow :inherit 'fixed-pitch))
     (org-level-4  (:foreground high-olive :inherit 'fixed-pitch))
     (org-level-5  (:foreground high-cyan :inherit 'fixed-pitch))
     (org-level-6  (:foreground high-purple :inherit 'fixed-pitch))
     (org-level-7  (:foreground green :inherit 'fixed-pitch))
     (org-level-8  (:foreground orange :inherit 'fixed-pitch))
     (org-drawer   (:foreground grey7 :inherit 'fixed-pitch))
     (org-tag      (:weight 'bold :inherit 'fixed-pitch))
     (org-link     (:inherit 'fixed-pitch :underline (:style 'line :color grey9)))
     (org-todo     (:foreground yellow :inherit 'fixed-pitch))
     (org-code     (:inherit 'fixed-pitch))
     (org-table    (:inherit 'fixed-pitch :foreground grey12))
     (org-block    (:inherit 'fixed-pitch))
     (org-footnote (:inherit 'fixed-pitch :foreground yellow))
     (org-checkbox (:inherit 'fixed-pitch))
     (org-checkbox-statistics-todo (:foreground higher-purple))
     (org-checkbox-statistics-done (:foreground high-cyan))

     (jupyter-repl-input-prompt  (:foreground olive))
     (jupyter-repl-output-prompt (:foreground red))

     (dashboard-items-face  (:foreground grey13))
     (dashboard-main-button (:foreground grey13 :bold t))

     ;; orderless
     (orderless-match-face-0 (:foreground high-cyan :bold t))
     (orderless-match-face-1 (:foreground high-olive :bold t))
     (orderless-match-face-2 (:foreground high-orange :bold t))
     (orderless-match-face-3 (:foreground high-yellow :bold t))

     (rainbow-delimiters-depth-1-face   (:foreground cyan))
     (rainbow-delimiters-depth-2-face   (:foreground purple))
     (rainbow-delimiters-depth-3-face   (:foreground green))
     (rainbow-delimiters-depth-4-face   (:foreground orange))
     (rainbow-delimiters-depth-5-face   (:foreground cyan))
     (rainbow-delimiters-depth-6-face   (:foreground purple))
     (rainbow-delimiters-depth-7-face   (:foreground green))
     (rainbow-delimiters-depth-8-face   (:foreground orange))
     (rainbow-delimiters-depth-9-face   (:foreground cyan))
     (rainbow-delimiters-depth-10-face  (:foreground purple))
     (rainbow-delimiters-depth-11-face  (:foreground green))
     (rainbow-delimiters-depth-12-face  (:foreground orange))
     (rainbow-delimiters-unmatched-face (:foreground high-red))

     ;; eshell
     (eshell-prompt        (:foreground cyan))
     (eshell-ls-archive    (:foreground grey7))
     (eshell-ls-backup     (:foreground grey7))
     (eshell-ls-clutter    (:foreground orange :bold t))
     (eshell-ls-directory  (:foreground yellow))
     (eshell-ls-executable (:bold t))
     (eshell-ls-missing    (:foreground high-red :bold t))
     (eshell-ls-product    (:foreground low-red))
     (eshell-ls-readonly   (:foreground grey7))
     (eshell-ls-special    (:foreground high-yellow :bold t))
     (eshell-ls-symlink    (:foreground high-red))
     (eshell-ls-unreadable (:foreground high-red :bold t))

     ;; lsp
     (lsp-face-highlight-textual        (:background grey6))
     (lsp-face-highlight-read           (:background grey6))
     (lsp-face-highlight-write          (:background grey6))
     (lsp-face-semhl-constant           (:foreground cyan))
     (lsp-face-semhl-deprecated         (:foreground cyan))
     (lsp-face-semhl-enummember         (:foreground cyan))
     (lsp-face-semhl-field              (:foreground cyan))
     (lsp-face-semhl-field-static       (:foreground cyan))
     (lsp-face-semhl-function           (:foreground cyan))
     (lsp-face-semhl-method             (:foreground cyan))
     (lsp-face-semhl-namespace          (:foreground cyan))
     (lsp-face-semhl-preprocessor       (:foreground cyan))
     (lsp-face-semhl-static-method      (:foreground cyan))
     (lsp-face-semhl-type-class         (:foreground cyan))
     (lsp-face-semhl-type-enum          (:foreground cyan))
     (lsp-face-semhl-type-primitive     (:foreground cyan))
     (lsp-face-semhl-type-template      (:foreground cyan))
     (lsp-face-semhl-type-typedef       (:foreground cyan))
     (lsp-face-semhl-variable           (:foreground cyan))
     (lsp-face-semhl-variable-local     (:foreground cyan))
     (lsp-face-semhl-variable-parameter (:foreground cyan))
     (lsp-lens-face                     (:foreground cyan))
     (lsp-lens-mouse-face               (:foreground cyan))
     (lsp-ui-doc-background             (:foreground cyan))
     (lsp-ui-doc-header                 (:foreground cyan))
     (lsp-ui-doc-url                    (:foreground cyan))
     (lsp-ui-peek-filename              (:foreground cyan))
     (lsp-ui-peek-footer                (:foreground cyan))
     (lsp-ui-peek-header                (:foreground cyan))
     (lsp-ui-peek-highlight             (:foreground cyan))
     (lsp-ui-peek-line-number           (:foreground cyan))
     (lsp-ui-peek-list                  (:foreground cyan))
     (lsp-ui-peek-peek                  (:foreground cyan))
     (lsp-ui-peek-selection             (:foreground cyan))
     (lsp-ui-sideline-code-action       (:foreground cyan))
     (lsp-ui-sideline-current-symbol    (:foreground cyan))
     (lsp-ui-sideline-symbol            (:foreground cyan))
     (lsp-ui-sideline-symbol-info       (:foreground cyan))

     ;; flymake
     (flymake-error (:underline (:style 'wave :color high-red)))
     (flymake-warning (:underline (:style 'wave :color high-cyan)))

     ;; flyspell
     (flyspell-incorrect (:underline (:style 'wave :color red)))

     ;; git-gutter
     (git-gutter:added (:foreground higher-green :weight 'bold))
     (git-gutter:modified (:foreground higher-purple :weight 'bold))
     (git-gutter:deleted (:foreground higher-red :weight 'bold))

     ;; other
     (mini-modeline-mode-line  (:background low-olive :height 0.14 :box nil))
     (yeet/mode-line-meow-mode (:foreground higher-olive :background grey4))
     (whitespace-tab      (:background bg :foreground grey6))
     (whitespace-trailing (:background high-red))
     (show-paren-match    (:background cyan))
     (help-key-binding    (:foreground high-orange :background grey4 :box (:line-width 1 :color grey6)))
     (escape-glyph        (:foreground higher-purple))
     (dired-broken-symlink (:foreground grey1 :background red :bold t))
     (dired-symlink (:foreground cyan))
     (dired-directory (:foreground orange))

     (isearch (:foreground grey13 :background high-purple))

     (sly-warning-face (:underline (:color high-orange)))

     (minimap-active-region (:background grey10))

     (meow-normal-indicator (:foreground higher-olive))
     (meow-insert-indicator (:foreground higher-red))
     (meow-beacon-indicator (:foreground higher-cyan))
     (meow-keypad-indicator (:foreground higher-purple))
     (meow-motion-indicator (:foreground higher-green))
     (meow-search-indicator (:foreground higher-yellow))

     (vterm-color-cyan    (:foreground cyan))
     (vterm-color-green   (:foreground olive))
     (vterm-color-black   (:foreground grey2))
     (vterm-color-blue    (:foreground orange))
     (vterm-color-red     (:foreground red))
     (vterm-color-yellow  (:foreground yellow))
     (vterm-color-white   (:foreground grey11))
     (vterm-color-magenta (:foreground purple))

     (csv-separator-face (:foreground high-cyan))
     )
    ,@body))

(provide 'battery)
