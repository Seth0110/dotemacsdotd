;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ace-jump-mode)
(use-package bison-mode)
(use-package csharp-mode)
(use-package docker)
(use-package dockerfile-mode)
(use-package geiser)
(use-package geiser-mit)
(use-package gnuplot-mode)
(use-package haskell-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package pdf-tools
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook 'pdf-view-mode (lambda () (display-line-numbers-mode 0))))
(use-package racket-mode)
(use-package shakespeare-mode)
(use-package srcery-theme)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package yaml-mode)

(if (display-graphic-p)
    (load-theme 'srcery 1))
(global-set-key (kbd "C-c ,") 'ace-jump-mode)

;; pdf-tools as default PDF viewer
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(pdf-tools-install)
