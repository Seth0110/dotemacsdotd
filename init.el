;;;; My personal Emacs configuration file.

;;; Set up package management
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;;; Color scheme
(use-package almost-mono-themes)
(if (not (window-system))
    (load-theme 'adwaita)
  (load-theme 'almost-mono-cream t))


;;; Window settings
(use-package golden-ratio)
(global-set-key (kbd "C-S-g") 'golden-ratio)

;;; Text settings
(show-paren-mode 1)
(defvar show-paren-delay 0)
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :height 140)
(set-language-environment 'utf-8)
(add-hook 'text-mode-hook 'line-number-mode)
(use-package csv-mode
  :init
  (add-hook 'csv-mode 'csv-align-mode))
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package olivetti
  :config (setq olivetti-body-width 80))

;;; Code settings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;;; Autocomplete
(use-package company)

;;; Compilation
(global-set-key [f5] 'compile)
(setq compilation-always-kill t)

;;; C
(use-package column-enforce-mode)
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

;;; Elisp & Eshell
(require 'em-tramp)
(add-to-list 'eshell-modules-list 'eshell-tramp)

;;; Go
(use-package go-mode)
(add-hook 'go-mode-hook 'column-enforce-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 "go build .")))

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

;;; SageMath
(use-package sage-shell-mode)
(use-package ob-sagemath)

;;; Various languages
(use-package bison-mode)
(use-package dockerfile-mode)
(use-package gnuplot)
(use-package haskell-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package nginx-mode)
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
(defun eww-read ()
  (interactive)
  (progn
    (olivetti-mode)
    (eww-readable)))
(define-key eww-mode-map (kbd "o") 'eww-read)

;;; Annoying settings
(global-unset-key (kbd "C-z"))
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq custom-file "~/.emacs.d/custom.el")
(setq ring-bell-function 'ignore)

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

;;; Revision control
(use-package magit)

;;; Org mode
;; (add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-pretty-entities t)
(use-package org-appear
    :hook (org-mode . org-appear-mode))
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
      (list "~/Documents/org/work.org"))
(setq org-return-follows-link t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (gnuplot . t)
   (haskell . t)
   (python . t)
   (sagemath . t)
   ))

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

;;; Efficiency
(use-package ace-jump-mode)
(bind-key "C-." 'ace-jump-mode)
(use-package which-key)
(which-key-mode)

;;; Work stuff
(if (file-exists-p "~/.emacs.d/work.el")
    (load-file "~/.emacs.d/work.el"))
