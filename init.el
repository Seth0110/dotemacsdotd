;; Set up package management
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Various display modes
(display-time-mode)
(display-battery-mode)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)

;; Font
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "FreeMono 16")
  (set-face-attribute 'fixed-pitch nil :font "FreeMono 16")
  (set-face-attribute 'variable-pitch nil :font "CMU Serif 16"))

;; Column & line numbers
(column-number-mode t)
(cond ((display-graphic-p)
       (global-hl-line-mode 1)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Text settings
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

;; Lisp
(use-package rainbow-delimiters
  :hook emacs-lisp-mode-hook)

;; Python
(defvar python-shell-interpreter "python3")
(use-package pyvenv)
(use-package pyvenv-auto)

;; CSharp
(use-package csharp-mode)
(setq c-default-style "linux"
      c-basic-offset 4)

;; Haskell
(use-package haskell-mode
  :hook
  (haskell-mode-hook . (lambda () (setq compile-command "stack build"))))
(use-package shakespeare-mode)
(defun haskell-mode-setup ()
  (setq haskell-process-type 'stack-ghci))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-mode-setup)
(custom-set-variables '(haskell-process-type 'stack-ghci))
(define-skeleton s/h-l
  "Write a Haskell language extension."
  "LANGUAGE: "
  "{-# LANGUAGE " str " #-}")

;; Various languages
(use-package bison-mode)
(use-package dockerfile-mode)
(use-package gnuplot)
(use-package haskell-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package nginx-mode)
(use-package sage-shell-mode)
(use-package yaml-mode)

;; Enable "advanced" features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Directory
(setq dired-listing-switches "-alFh")
(setq tramp-default-method "ssh")
(setq epa-pinentry-mode 'loopback)

;; Web
(setq eww-search-prefix "https://duckduckgo.com/lite/?q=")
(setq browse-url-browser-function 'eww-browse-url)
(defun eww-read ()
  (interactive)
  (progn
    (olivetti-mode)
    (eww-readable)))
(define-key eww-mode-map (kbd "o") 'eww-read)
(defun firefox ()
  "Launch firefox without a corresponding buffer."
  (interactive)
  (start-process "firefox" nil "firefox"))

;; RSS
(use-package elfeed)
(use-package elfeed-dashboard)
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Documents/org/elfeed.org")))

;; Annoying settings
(setq custom-file "~/.emacs.d/custom.el")
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))

;; PDF
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

;; Uptimes
(use-package uptimes)

;; Revision control
(use-package magit)

;; Org mode
(add-hook 'org-mode-hook 'org-indent-mode)
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
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config (setq org-auto-tangle-default t))
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

;; Custom functions
(defun reload ()
  "Reload the init file without restarting"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun ask-before-closing ()
  "Ask if you really want to quit"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to blaspheme the sacred editor? "))
      (save-buffers-kill-emacs)                                                                                          (message "That's what I thought.")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(defun launch ()
  (interactive)
  (let ((cmd (read-string "Command: ")))
    (start-process cmd nil cmd)))

;; Movement
(use-package ace-jump-mode)
(bind-key "C-." 'ace-jump-mode)

;; Work stuff
(if (file-exists-p "~/.emacs.d/work.el")
    (load-file "~/.emacs.d/work.el"))
