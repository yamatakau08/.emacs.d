(defun ffmpegcomp (file)
  "compress movie file 1/2 to file_comp.mp4 using ffmpeg
ffmpeg -i file.mp4 -crf 30 file_comp.mp4"
  (interactive "fmpegfile: ")
  (if (member-ignore-case (file-name-extension file) '("mp4" "MOV"))
      (let* ((ffmpeg (if (eq (window-system) 'w32)
			 "c:/winbin/ffmpeg-master-latest-win64-gpl-shared/bin/ffmpeg.exe" ; /cygdrive/ notifiicatio is also available
		       "ffmpeg"))
	     (outfile (concat (file-name-directory file)
			      (file-name-base file)
			      "_comp."
			      (file-name-extension file)))
	     (crfvalue "28") ; constant rate factor
	     (cmd (mapconcat #'shell-quote-argument
			     ;; ffmpeg option -n never overwrite output files
			     (list ffmpeg "-n" "-i" file "-crf" crfvalue outfile) " "))
	     cmdoutput)
	(message "%s" cmd)
	;; if output file already exisit, ffmpeg outputs "... already exists. Exiting."
	(setq cmdoutput (shell-command-to-string cmd))
	(if (string-match "already exists. Exiting" cmdoutput)
	    (message "%s already exists." outfile)
	  (message "output %s" outfile))
	)
    (message "specify mp4file!")
    ))

(provide 'ffmpegcomp)
