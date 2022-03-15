(defun set-keys (keypairs)
	(dolist (keypair keypairs)
    (global-set-key (kbd (car keypair)) (cdr keypair))))

(defun reload-config ()
	"Hot reloads the user configuration"
	(interactive)
	(load
	 (expand-file-name "init.el" user-emacs-directory)))

(defconst journal-directory "/home/goiabae/note/date/")
(defun journal-open-today ()
	(interactive)
	(find-file (concat
							journal-directory
							(format-time-string "%d-%m-%Y")
							".org")))

	(defun meow-setup ()
		(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
		(meow-motion-overwrite-define-key
		 '("j" . meow-next)
		 '("k" . meow-prev)
		 '("<escape>" . ignore))
		(meow-leader-define-key
		 '("f" . "C-x F")
		 '("b" . "C-x B")
		 '("w" . "C-x w")
		 '("t" . "C-x T")
		 '("j" . "C-x j")
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
		 '("a" . meow-append)
		 '("A" . meow-append)
		 '("b" . meow-back-word)
		 '("B" . meow-back-symbol)
		 '("c" . meow-change)
		 '("d" . meow-delete)
		 '("D" . meow-backward-delete)
		 '("e" . meow-next-word)
		 '("E" . meow-next-symbol)
		 '("f" . meow-find)
		 '("g" . meow-cancel-selection)
		 '("G" . meow-grab)
		 '("h" . meow-left)
		 '("H" . meow-left-expand)
		 '("i" . meow-insert)
		 '("I" . meow-open-above)
		 '("j" . meow-next)
		 '("J" . meow-next-expand)
		 '("k" . meow-prev)
		 '("K" . meow-prev-expand)
		 '("l" . meow-right)
		 '("L" . meow-right-expand)
		 '("m" . meow-join)
		 '("n" . meow-search)
		 '("o" . meow-open-below)
		 '("O" . meow-to-block)
		 '("p" . meow-yank)
		 '("q" . meow-quit)
		 '("Q" . meow-goto-line)
		 '("r" . meow-replace)
		 '("R" . meow-swap-grab)
		 '("s" . meow-kill)
		 '("t" . meow-till)
		 '("u" . meow-undo)
		 '("U" . meow-undo-in-selection)
		 '("V" . meow-line)
		 '("v" . meow-visit)
		 '("w" . meow-mark-word)
		 '("W" . meow-mark-symbol)
		 '("x" . meow-line)
		 '("X" . meow-goto-line)
		 '("y" . meow-save) ; this really is meow-yank
		 '("Y" . meow-sync-grab)
		 '("z" . meow-pop-selection)
		 '("'" . repeat)
		 '("<escape>" . ignore)))

(defun eshell/sudo-open (filename)
	"Open a file as root in Eshell."
	(let ((qual-filename (if (string-match "^/" filename)
													 filename
												 (concat (expand-file-name (eshell/pwd)) "/" filename))))
		(switch-to-buffer
		 (find-file-noselect
			(concat "/sudo::" qual-filename)))))
