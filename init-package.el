;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ace-jump-mode)
(use-package csharp-mode)
(use-package docker)
(use-package dockerfile-mode)
(use-package geiser)
(use-package geiser-mit)
(use-package gnuplot-mode)
(use-package haskell-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package sourcerer-theme)
(use-package shakespeare-mode)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package yaml-mode)

(global-set-key (kbd "C-c ,") 'ace-jump-mode)
