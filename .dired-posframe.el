(use-package dired-posframe
  :ensure t
  :config
  (custom-set-variables '(dired-posframe-file-size-limit (* 10 1024 1024)))

  :bind (("z" . dired-posframe-show))
  )

;(add-hook 'dired-mode-hook
;	  (lambda ()
;	    (define-key dired-mode-map "z" 'dired-posframe-show)))

;(defun peep-dired--image-p (path)
;  "check if file is a kind of image or text"
;  ;;(string-match "\\(image\\|text\\)"
;  (string-match "image"
;		(shell-command-to-string
;		 (mapconcat #'shell-quote-argument
;			    (list "file" path)
;			    " "))))
;
;(defun dired-posframe--show ()
;  "Show dired-posframe for current dired item."
;  (let ((path (dired-get-filename nil 'noerror))
;        hide)
;    (with-current-buffer (get-buffer-create dired-posframe-buffer)
;      (let ((inhibit-read-only t))
;        (erase-buffer)
;        (cond
;         ((not path)
;          (setq hide t))
;         ((file-directory-p path)
;          (insert (with-current-buffer (dired-noselect path)
;                    (buffer-substring-no-properties (point-min) (point-max)))))
;         ((file-readable-p path)
;	  (if (peep-dired--image-p path)
;	      (insert-file-contents path)
;	    (insert (format "not supported")))
;
;          ))
;        (when path
;          (let* ((name (file-name-nondirectory path))
;                 (mode (assoc-default name auto-mode-alist #'string-match)))
;            (if (memq mode dired-posframe-enable-modes)
;                (set-auto-mode-0 mode)
;              (set-auto-mode-0 'fundamental-mode 'keep-mode-if-same))))))
;    (if hide
;        (dired-posframe--hide)
;      (if (peep-dired--image-p path)
;	  (dired-posframe-display dired-posframe-buffer)))))
