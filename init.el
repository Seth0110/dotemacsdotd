;;;; My personal Emacs configuration file.

;;; Annoying settings
(global-unset-key (kbd "C-z"))
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq custom-file "~/.emacs.d/custom.el")
(setq ring-bell-function 'ignore)

;;; Set up package management
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Custom functions
(defun reload ()
  "Reload the init file without restarting"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun ask-before-closing ()
  "Ask if you really want to quit"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to blaspheme the sacred editor? "))
      (save-buffers-kill-emacs)
    (message "That's what I thought.")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(defun decide ()
  "Decide between a yes or a no"
  (interactive)
  (prin1 (if (zerop (random 2))
	     'yes 'no)))

;;; Theme
(use-package plan9-theme)
(load-theme 'plan9 t)

;;; Various display modes
(display-time-mode)
(display-battery-mode)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)

;;; Font
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;;; Column & line numbers
(column-number-mode t)
(cond ((display-graphic-p)
       (global-hl-line-mode 1)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Window settings
(use-package golden-ratio)
(global-set-key (kbd "C-S-g") 'golden-ratio)

;;; Text settings
(show-paren-mode 1)
(defvar show-paren-delay 0)
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :height 180)
(set-language-environment 'utf-8)
(add-hook 'text-mode-hook 'line-number-mode)
(use-package csv-mode
  :init
  (add-hook 'csv-mode 'csv-align-mode))
(use-package visual-regexp)
(use-package visual-regexp-steroids)

;;; Code settings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;;; Autocomplete
(use-package company)

;;; Compilation
(global-set-key [f5] 'compile)
(setq compilation-always-kill t)

;;; C
(use-package column-enforce-mode)
(use-package auto-header)
(add-hook 'c-mode-hook 'column-enforce-mode)
;;;; Linux kernel coding style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

(setq c-default-style "linux-tabs-only"
      c-basic-offset 8)

;;; Elisp
(require 'em-tramp)
(add-to-list 'eshell-modules-list 'eshell-tramp)
(use-package async)

;;; Go
(use-package go-mode)
(add-hook 'go-mode-hook 'column-enforce-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;;; Haskell
(use-package company-ghci)
(use-package haskell-mode)
(use-package hindent)
(use-package shakespeare-mode)
(setq haskell-process-type 'stack-ghci)
(custom-set-variables '(haskell-process-type 'stack-ghci))
(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-mode-setup)
(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 "stack build")))

;;; Python
(defvar python-shell-interpreter "python3")
(use-package pyvenv)
(use-package pyvenv-auto)

;;; Web
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.js?\\'" . web-mode)
   ("\\.css?\\'" . web-mode)
   ("\\.xml?\\'" . web-mode)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Various languages
(use-package bison-mode)
(use-package dockerfile-mode)
(use-package gnuplot)
(use-package json-mode)
(use-package markdown-mode)
(use-package yaml-mode)

;;; Enable "advanced" features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Directory
(setq dired-listing-switches "-alFh")
(setq epa-pinentry-mode 'loopback)
(setq tramp-default-method "ssh")

;;; Web
(setq eww-search-prefix "https://duckduckgo.com/lite/?q=")
(setq browse-url-browser-function 'eww-browse-url)
(use-package wttrin
  :custom
  (wttrin-default-locations '("East Syracuse")))

;;; RSS
(use-package elfeed)
(setq elfeed-curl-extra-arguments '("--insecure"))
(setq elfeed-feeds '("https://stallman.org/rss/rss.xml"
		     "https://lukesmith.xyz/index.xml"))

;;; PDF
(defun set-pdf-tools ()
  "Set pdf-tools as the default PDF viewer"
  (if (eq system-type 'gnu/linux)
      (progn
        (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
              TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
              TeX-source-correlate-start-server t)
        (add-hook 'TeX-after-compilation-finished-functions
                  #'TeX-revert-document-buffer)
        (pdf-tools-install))))
(use-package pdf-tools
    :config
    (set-pdf-tools))

;;; Org mode
(require 'org)
;; (add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-pretty-entities t)
(defun fix-org-mode-levels ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'fix-org-mode-levels)
(setq org-agenda-files
      (list "~/org/notes.org"))
(setq org-return-follows-link t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-confirm-babel-evaluate nil)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (shell . t)))

;;; Efficiency
(use-package ace-jump-mode)
(bind-key "C-." 'ace-jump-mode)
(use-package which-key)
(which-key-mode)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Work stuff
(if (file-exists-p "~/.emacs.d/work.el")
    (load-file "~/.emacs.d/work.el"))
