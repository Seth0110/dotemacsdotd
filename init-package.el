;; Package management
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
(use-package company
  :hook (prog-mode . company-mode))
(use-package geiser)
(use-package haskell-mode)
(use-package jinja2-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (cua-mode 0)))
  (setq pdf-view-resize-factor 1.1))
;; (use-package sourcerer-theme)
(use-package terraform-mode)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package yaml-mode)
