(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(display-time-mode)
(display-battery-mode)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)

(column-number-mode t)
(cond ((display-graphic-p)
       (global-hl-line-mode 1)))
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(use-package srcery-theme)

(defun dark ()
  "Enable dark mode"
  (interactive)
  (load-theme 'srcery))

(defun light ()
  "Enable light mode"
  (interactive)
  (progn
    (disable-theme 'srcery)
    (if (eq system-type 'gnu/linux)
        (set-frame-font "CMU Typewriter Text 18" nil t))))

(show-paren-mode 1)
(defvar show-paren-delay 0)
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :height 140)
(set-language-environment 'utf-8)
(add-hook 'text-mode-hook 'line-number-mode)
(if (eq system-type 'gnu/linux)
    (set-frame-font "CMU Typewriter Text 18" nil t))
(use-package csv-mode
  :init
  (add-hook 'csv-mode 'csv-align-mode))
(add-hook 'text-mode-hook 'flyspell-mode)

(use-package geiser)
(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package rainbow-delimiters
  :config
  :hook emacs-lisp-mode-hook)
(use-package sly)

(defvar python-shell-interpreter "python3")

(use-package csharp-mode)
(setq c-default-style "linux"
      c-basic-offset 4)

(use-package bison-mode)
(use-package dockerfile-mode)
(use-package gnuplot)
(use-package haskell-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package yaml-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq dired-listing-switches "-alFh")

(setq tramp-default-method "ssh")
(setq epa-pinentry-mode 'loopback)

(setq eww-search-prefix "https://duckduckgo.com/lite/?q=")
(setq browse-url-browser-function 'eww-browse-url)

(use-package elfeed
  :config
  (defalias 'elfeed-youtube
    (kmacro "<return> C-n C-n C-n C-n C-n M-f M-f <return> & q q C-p")
    "From the elfeed screen go down to the URL and open it in the external browser.")
  (define-key elfeed-search-mode-map "y" 'elfeed-youtube))
(use-package elfeed-dashboard)
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Documents/org/elfeed.org")))

(setq custom-file "~/.emacs.d/custom.el")
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))

(use-package pdf-tools
  :config
  (set-pdf-tools))
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

(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))
(setq eshell-prompt-function
      (lambda ()
        (concat (fish-path (eshell/pwd) 40)
                (if (= (user-uid) 0) " # " " λ "))))

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil))
(use-package uptimes)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-agenda-files
      (list "~/Documents/org/work.org"))
(setq org-agenda-start-with-log-mode t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NEXT" . "goldenrod")
        ("WAIT" . "orange")
        ("CANCELLED" . "blue")))
(setq org-log-done 'time)
(use-package org-pomodoro)

(setq org-return-follows-link t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(use-package company)
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config (setq org-auto-tangle-default t))
(defun set-compile-key ()
  (local-set-key (kbd "<f5>") 'compile))
(add-hook 'prog-mode-hook 'set-compile-key)
(add-hook 'org-auto-tangle-mode-hook 'set-compile-key)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'sly-mode-hook 'company-mode)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

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
(use-package ace-jump-mode)
(define-key global-map (kbd "C-.") 'ace-jump-mode)

(define-skeleton s/h-l
  "Write a Haskell language extension."
  "LANGUAGE: "
  "{-# LANGUAGE " str " #-}")

(if (file-exists-p "init-work.el")
    (load-file "~/.emacs.d/init-work.el"))
