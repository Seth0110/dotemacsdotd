;; Backup settings

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Menu settings
(display-time-mode)
(display-battery-mode)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)

;; Line settings
(column-number-mode t)
(line-number-mode t)
(cond ((display-graphic-p)
       (global-hl-line-mode 1)))
(defvar linum-modes
      '(prog-mode-hook
	text-mode-hook))
(dolist (mode linum-modes)
  (add-hook mode 'display-line-numbers-mode))

;; Text settings
(show-paren-mode 1)
(defvar show-paren-delay 0)
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :height 140)
(set-language-environment 'utf-8)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'line-number-mode)

;; Misc
(setq ring-bell-function 'ignore)
(defvar python-shell-interpreter "python3")
(setq tramp-default-method "ssh")

(load-file "~/.emacs.d/init-package.el")
