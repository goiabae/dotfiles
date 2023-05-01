(setq package-enable-at-startup nil
      package-archives nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
    (lambda ()
      (setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1)))

(setq inhibit-startup-screen t
      inhibit-default-init t
      inhibit-startup-buffer-menu t)

(setq frame-inhibit-implied-resize t)

(setq site-run-file nil)
