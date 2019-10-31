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
	   '("\\.pptx" nil (file)) ; nil: Since "open" in specified in openwith.el selects application associated with file suffix, we don't need to specify application program.
	   (list (openwith-make-extension-regexp
		  '("xlsx" "xlsm"))
		 nil
		 '(file))))
    )
   ((eq system-type 'darwin)
    (setq openwith-associations
	  (list
	   '("\\.pdf"     "open" (file))
	   '("\\.numbers" "open" (file))))
    )
   )
  (openwith-mode 1))

;;;
(defun openwith-open-windows-filter-args (arg)
  ;; this filter advice function
  ;; arg is list ("//jps00004944/share_data/SQA1-HA/10_Test_Project_FY17/NewAudio/02_検証計画書/Schedule-NewAudio-Evaluation.pptx")
  ;; replace file path in car of arg with my-app-open-file-path2explore
  ;;; "\\\\jps00004944\\share_data\\SQA1-HA\\10_Test_Project_FY17\\NewAudio\\02_検証計画書\\Schedule-NewAudio-Evaluation.pptx"
  (list (my-app-open-file-path2explore (car arg)))
  )

(advice-add #'openwith-open-windows :filter-args
	    #'openwith-open-windows-filter-args)
