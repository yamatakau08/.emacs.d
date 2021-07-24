;;;
(use-package openwith
  :ensure t
  )

;;; refer https://github.com/emacsmirror/openwith/blob/master/README.txt
(when (require 'openwith nil 'noerror)
  (cond
   ((eq system-type 'windows-nt)
    (setq openwith-associations
	  (list
	   ;; '("\\.png"  "open" (file)) ; open is also ok.
	   ;; '("\\.pptx" nil    (file)) ; nil: Since "open" in specified in openwith.el selects application associated with file suffix, we don't need to specify application program.
	   ;; office
	   (list (openwith-make-extension-regexp
		  '("pptx" "xlsx" "xlsm" "docx"))
		 nil
		 '(file))
	   ;; other than office
	   (list (openwith-make-extension-regexp
		  '("pdf" "msg" "jpg" "png" "mp3" "mp4"))
		 nil
		 '(file))))
    )
   ((eq system-type 'darwin)
    ;; need to study beep sound is heared when open file
    (setq openwith-associations
	  (list
	   '("\\.pdf"     "open" (file))
	   '("\\.numbers" "open" (file))))
    )
   )
  (openwith-mode 1))

;; filter-args
(defun openwith-open-windows-filter-args (arg)
  (message "openwith-open-windows arg: %s" arg) ; for debug

  ;; arg is list ("//jps00004944/share_data/SQA1-HA/10_Test_Project_FY17/NewAudio/02_検証計画書/Schedule-NewAudio-Evaluation.pptx")
  ;; The file path in car of arg SHOULD be converted '/' to '\' with my-app-open-file-path2explore
  ;; expected result is the following
  ;; "\\\\jps00004944\\share_data\\SQA1-HA\\10_Test_Project_FY17\\NewAudio\\02_検証計画書\\Schedule-NewAudio-Evaluation.pptx"
  (list (my-app-open-file-path2explore (car arg)))
  )

;; advice flter-args
(advice-add #'openwith-open-windows
	    :filter-args
	    #'openwith-open-windows-filter-args)

;; for dired
(defun my-openwith-oepn-windows-dired-winstart ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
	(cond
	 ((eq system-type 'windows-nt)
	  ;; (w32-shell-execute "open" fname) ; pass same as openwith-open-windows funcion
	  ;; (openwith-file-handler (insert-file-contents (convert-standard-filename fname))) ;fail
	  (openwith-open-windows fname))
	 ((eq system-type 'darwin)
	  ;; (shell-command-to-string (format "open %s" fname))))
	  (start-process "open-with-default-app" nil "open" fname))))))

;;
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map "z" 'my-openwith-oepn-windows-dired-winstart)))

;; redefine
;; original opewith-file-handler has beep sound
;; I give up to use openwith
(defun openwith-file-handler (operation &rest args)
  "Open file with external program, if an association is configured."
  (catch 'assocs-found
    (when (and openwith-mode (not (buffer-modified-p)) (zerop (buffer-size)))
      (let ((assocs openwith-associations)
	    (file (car args))
	    oa)
	;; do not use `dolist' here, since some packages (like cl)
	;; temporarily unbind it
	(while assocs
	  (setq oa (car assocs)
		assocs (cdr assocs))
	  (when (save-match-data (string-match (car oa) file))
	    (let ((params (mapcar (lambda (x) (if (eq x 'file) file x))
				  (nth 2 oa))))
	      (when (or (not openwith-confirm-invocation)
			(y-or-n-p (format "%s %s? " (cadr oa)
					  (mapconcat #'identity params " "))))
		(if (eq system-type 'windows-nt)
		    (openwith-open-windows file)
		  (openwith-open-unix (cadr oa) params))
		(when (featurep 'recentf)
		  (recentf-add-file file))
		;; inhibit further actions
		(message "Opened %s in external program"
			 ;; original error, since beep sound occur,changed to message.
			 ;; but beep still persists, somehow file-truename(nil) error has occcured
			 (file-name-nondirectory file))
		(kill-buffer nil) ; takaxp advice, but no effects
		(throw 'assocs-found t)
		))))))
    ;; when no association was found, relay the operation to other handlers
    (let ((inhibit-file-name-handlers
	   (cons 'openwith-file-handler
		 (and (eq inhibit-file-name-operation operation)
		      inhibit-file-name-handlers)))
	  (inhibit-file-name-operation operation))
      (apply operation args))))
