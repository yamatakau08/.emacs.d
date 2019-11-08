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
    ;; need to study alert sound is heared when open file
    (setq openwith-associations
	  (list
	   '("\\.pdf"     "open" (file))
	   '("\\.numbers" "open" (file))))
    )
   )
  (openwith-mode 1))

;; filter-args
(defun openwith-open-windows-filter-args (arg)
  ;; this filter advice function
  ;; arg is list ("//jps00004944/share_data/SQA1-HA/10_Test_Project_FY17/NewAudio/02_検証計画書/Schedule-NewAudio-Evaluation.pptx")
  ;; replace file path converted with my-app-open-file-path2explore in car of arg list
  ;;; "\\\\jps00004944\\share_data\\SQA1-HA\\10_Test_Project_FY17\\NewAudio\\02_検証計画書\\Schedule-NewAudio-Evaluation.pptx"
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
