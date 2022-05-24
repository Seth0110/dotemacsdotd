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
;; (defvar linum-modes
;;       '(prog-mode-hook
;; 	text-mode-hook))
;; (dolist (mode linum-modes)
;;   (add-hook mode 'display-line-numbers-mode))
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; Text settings
(show-paren-mode 1)
(defvar show-paren-delay 0)
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :height 140)
(set-language-environment 'utf-8)
;; (add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'line-number-mode)
(set-frame-font "CMU Typewriter Text 18" nil t)

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

;; Load more files
(load-file "~/.emacs.d/init-skeleton.el")
(load-file "~/.emacs.d/init-package.el")
(load-file "~/.emacs.d/init-custom-commands.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8da34297ccd16aa9fdf75596dc06d519a5f036179fbff95107bbecdaadf965c4" "c5a81a42df109b02a9a68dfe0ed530080372c1a0bbcb374da77ee3a57e1be719" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "529c211e86eadecb67b6b64ffdf73e71c4337070bd9b3de053f8f7c5da9e07a2" default))
 '(package-selected-packages
   '(inkpot-theme ada-mode ack ace-window sokoban clojure-mode docker magit yaml-mode visual-regexp-steroids visual-regexp markdown-mode json-mode haskell-mode gnuplot-mode geiser-mit geiser elm-mode dockerfile-mode ace-jump-mode use-package))
 '(warning-suppress-types '((use-package) (emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
