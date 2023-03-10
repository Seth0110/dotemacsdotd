;; Agenda files
(setq org-agenda-files
      (list "~/Documents/org/work.org"))

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; TODO state colors
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("NEXT" . "goldenrod")
	("WAIT" . "orange")
	("CANCELLED" . "blue")))

;; Log a timestamp when a task is completed
(setq org-log-done 'time)

;; Nicer indentation
(add-hook 'org-mode-hook 'org-indent-mode)

;; Follow the links
(setq org-return-follows-link  t)

;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;; Wrap the lines in org mode so that things are easier to read
(add-hook 'org-mode-hook 'visual-line-mode)

;; Capture template for unorganized notes
(setq org-capture-templates
      '(("n" "Note"
         entry (file+headline "~/Documents/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)))
