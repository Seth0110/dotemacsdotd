;; Backup settings

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Menu settings
(display-time-mode)
(display-battery-mode)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)

;; Line settings
(column-number-mode t)
(cond ((display-graphic-p)
       (global-hl-line-mode 1)))
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Text settings
(show-paren-mode 1)
(defvar show-paren-delay 0)
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :height 140)
(set-language-environment 'utf-8)
;; (add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'line-number-mode)
(if (eq system-type 'gnu/linux)
    (set-frame-font "CMU Typewriter Text 18" nil t))

;; Enabled advanced features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Language-specific settings
(define-key prog-mode-map [f5] #'compile)
(setq c-default-style "linux"
      c-basic-offset 4)

;; Misc
(setq diary-list-include-blanks t)
(setq ring-bell-function 'ignore)
(defvar python-shell-interpreter "python3")
(setq tramp-default-method "ssh")
(setq epa-pinentry-mode 'loopback)
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")
(setq eww-search-prefix "https://duckduckgo.com/lite/?q=")
(setq browse-url-browser-function 'eww-browse-url)
;; (setq inhibit-startup-screen t)

;; I hate custom set variables!!!
(setq custom-file null-device)

;; Load more files
(load-file "~/.emacs.d/init-package.el")
(load-file "~/.emacs.d/init-org.el")
(load-file "~/.emacs.d/init-skeleton.el")
(load-file "~/.emacs.d/init-custom-commands.el")
(if (file-exists-p "init-work.el")
    (load-file "~/.emacs.d/init-work.el"))
