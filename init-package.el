;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;; Import packages
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
(use-package bison-mode)
(use-package csharp-mode)
(use-package csv-mode
  :init
  (add-hook 'csv-mode 'csv-align-mode))
(use-package dockerfile-mode)
(use-package elfeed)
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Documents/org/elfeed.org")))
(use-package esh-autosuggest)
(use-package eshell-syntax-highlighting)
(use-package flycheck)
(use-package flycheck-haskell)
(use-package geiser)
(use-package gnuplot-mode)
(use-package haskell-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package pdf-tools
  :config (set-pdf-tools))
(use-package rainbow-delimiters
  :config
  :hook emacs-lisp-mode-hook)
(use-package shakespeare-mode)
(use-package srcery-theme)
(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
		pomidor-sound-tack nil))
(use-package uptimes)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package yaml-mode)
