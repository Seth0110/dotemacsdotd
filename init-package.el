;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
(use-package base16-theme)
(if (display-graphic-p)
    (load-theme 'base16-solarized-light t))
(use-package company
  :hook (prog-mode . company-mode))
(use-package geiser)
(use-package haskell-mode)
(use-package jinja2-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package terraform-mode)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package yaml-mode)
(use-package jenkinsfile-mode)
