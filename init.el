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
;; (add-hook 'text-mode-hook 'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
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
(set-frame-font "CMU Typewriter Text 18" nil t)

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
;; (setq inhibit-startup-screen t)

;; Load more files
(load-file "~/.emacs.d/init-skeleton.el")
(load-file "~/.emacs.d/init-package.el")
(load-file "~/.emacs.d/init-custom-commands.el")
(if (file-exists-p "init-work.el")
    (load-file "~/.emacs.d/init-work.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "623e9fe0532cc3a0bb31929b991a16f72fad7ad9148ba2dc81e395cd70afc744" "a24e023c71f74e8c6a79068947d706f68a566b49a774096671a5cbfe1fb90fc6" "8da34297ccd16aa9fdf75596dc06d519a5f036179fbff95107bbecdaadf965c4" "c5a81a42df109b02a9a68dfe0ed530080372c1a0bbcb374da77ee3a57e1be719" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "529c211e86eadecb67b6b64ffdf73e71c4337070bd9b3de053f8f7c5da9e07a2" default))
 '(display-line-numbers t)
 '(elfeed-feeds
   '("https://lukesmith.xyz/index.xml" "https://www.youtube.com/feeds/videos.xml?channel_id=UCldfgbzNILYZA4dmDt4Cd6A" "https://www.youtube.com/feeds/videos.xml?channel_id=UCDRIjKy6eZOvKtOELtTdeUA"))
 '(ignored-local-variable-values
   '((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(package-selected-packages
   '(display-wttr wttrin pomidor empv emms eww-lnum unicode-fonts hasklig-mode pdf-tools mines afternoon-theme elf-mode advice-patch adjust-parens adaptive-wrap ada-ref-man inkpot-theme ada-mode ack ace-window sokoban clojure-mode docker yaml-mode visual-regexp-steroids visual-regexp markdown-mode json-mode haskell-mode gnuplot-mode geiser-mit geiser elm-mode dockerfile-mode ace-jump-mode use-package))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(warning-suppress-types '((use-package) (emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
