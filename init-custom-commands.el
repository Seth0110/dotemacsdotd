(defun reload ()
    (interactive)
    (load-file "~/.emacs.d/init.el"))

(defun ask-before-closing ()
  "Ask if you really want to quit"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to blaspheme the sacred editor? "))
      (save-buffers-kill-emacs)                                                                                          (message "That's what I thought.")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(defun dark ()
  (interactive)
  (load-theme 'srcery))

(defun light ()
  (interactive)
  (progn
   (disable-theme 'srcery)
   (if (eq system-type 'gnu/linux)
       (set-frame-font "CMU Typewriter Text 18" nil t))))
