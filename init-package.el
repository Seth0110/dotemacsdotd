;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Import packages
(use-package ace-jump-mode)
(use-package bison-mode)
(use-package csharp-mode)
(use-package dockerfile-mode)
(use-package elfeed)
(use-package flycheck)
(use-package flycheck-haskell)
(use-package geiser)
(use-package gnuplot-mode)
(use-package haskell-mode)
(use-package json-mode)
(use-package magit)
(use-package markdown-mode)
(use-package pdf-tools)
(use-package pomidor
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil))
(use-package shakespeare-mode)
(use-package solarized-theme)
(use-package srcery-theme)
(use-package uptimes)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package yaml-mode)

;; Load theme
(if (display-graphic-p)
    (load-theme 'solarized-selenized-light 1))

;; Ace jump mode config
(global-set-key (kbd "C-c ,") 'ace-jump-mode)

;; Set pdf-tools as the default PDF viewer
(if (eq system-type 'gnu/linux)
    (progn
      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	    TeX-source-correlate-start-server t)
      (add-hook 'TeX-after-compilation-finished-functions
		#'TeX-revert-document-buffer)
      (pdf-tools-install)))
