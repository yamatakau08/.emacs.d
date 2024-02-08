(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)

  :config
  ;; override original function to suppress the error ediff-files on Windows Emacs + cygwin
  (defun ediff-exec-process (program buffer synch options &rest files)
    "Execute the diff PROGRAM.

The PROGRAM output is sent to BUFFER, which must be a live buffer
object.

The PROGRAM is executed asynchronously unless `system-type' is
`windows-nt' or `ms-dos', or SYNCH is non-nil.

OPTIONS is a string of space-separated options to pass to PROGRAM.  It
may be a blank string.

FILES is a list of filenames to pass to PROGRAM; nil and \"\" elements
are ignored."
    (let ((data (match-data))
	  ;; If this is a buffer job, we are diffing temporary files
	  ;; produced by Emacs with ediff-coding-system-for-write, so
	  ;; use the same encoding to read the results.
	  (coding-system-for-read
	   (if (string-match "buffer" (symbol-name ediff-job-name))
	       ediff-coding-system-for-write
	     ediff-coding-system-for-read))
          (process-environment
           ;; Avoid localization of messages so we can parse the output.
           (cons "LC_MESSAGES=C" process-environment))
          args)
      (setq args (append (split-string options)
			 (mapcar (lambda (file)
				   (when (stringp file)
				     ;; workaround for the error ediff-files on Windows Emacs + Cygwin
				     ;; convert file-name encoding to cp932
				     (if (or (memq system-type '(ms-dos windows-nt)))
					 (encode-coding-string
					  (file-name-unquote
					   (or (file-local-copy file) file)) 'cp932)
				       (file-name-unquote
					(or (file-local-copy file) file)))))
				 files)))
      (setq args (delete "" (delq nil args))) ; delete nil and "" from arguments
      ;; the --binary option, if present, should be used only for buffer jobs
      ;; or for refining the differences
      (or (string-match "buffer" (symbol-name ediff-job-name))
	  (eq buffer ediff-fine-diff-buffer)
	  (setq args (delete "--binary" args)))
      (unwind-protect
          (with-current-buffer buffer
            (erase-buffer)
            ;; default-directory may be on some remote machine
            ;; (e.g. accessed via Tramp or url-handler) or a non-existing dir.
            (setq default-directory "/")
            (if (or (memq system-type '(ms-dos windows-nt))
                    synch)
		;; In Windows do it synchronously, since Windows doesn't let us
		;; delete files used by other processes. Thus, in ediff-buffers
		;; and similar functions, we can't delete temp files because
		;; they might be used by the asynch process that computes
		;; custom diffs. So, we have to wait till custom diff
		;; subprocess is done.
		;; In DOS, must synchronize because DOS doesn't have
		;; asynchronous processes.
		(apply #'call-process program nil buffer nil args)
              ;; On other systems, do it asynchronously.
              (let ((proc (get-buffer-process buffer)))
		(if proc (kill-process proc)))
	      (let ((proc
		     (apply #'start-process "Custom Diff" buffer program args)))
		(setq mode-line-process '(":%s"))
		(set-process-sentinel proc #'ediff-process-sentinel)
		(set-process-filter proc #'ediff-process-filter)
		)))
	(store-match-data data))))

  )

(provide '.ediff)
