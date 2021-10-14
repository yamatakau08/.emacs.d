;; the following message appears when execute C-x C-f or dired to open directory on Mac
;; ls does not support --dired; see ‘dired-use-ls-dired’ for more details.

;; dired-listing-switches
;; original value "-al"
;; (setq dired-listing-switches "-al")
;; switch to "-Alhv --group-directories-first"
;; have an error on Mac
;; insert-directory: Listing directory failed but ‘access-file’ worked
;(setq dired-listing-switches "-Alhv --group-directories-first")
;; on Windows
;  c:/yama/.emacs.d:
;  total used in directory 160 available 134740560
;    51509920738105335     4k drwxrwxrwx  1 0000910700 Domain Users     4k 12-23 17:10 .git
;     1125899907103619     2k -rw-rw-rw-  1 0000910700 Domain Users   1.6k 12-23 17:10 window-setting.el

(use-package dired
  :bind
  (:map dired-mode-map
	("C-l" . my-dired-explore-open)
	;; others are assigned in .consult.el dired-mode-map
	("C-j"      . my-dired-find-file)
	("<return>" . my-dired-find-file)
	)

  :config
  ;; original dired-get-marked-files function returns the file under the point when there is no marked files.
  (defun my-dired-get-marked-files ()
    (when (> (string-to-number (dired-number-of-marked-files)) 0)
      (dired-get-marked-files)))

  (defun my-dired-explore-open ()
    "open directory with Windows explore"
    (interactive)
    (if (eq (window-system) 'w32)
	(w32-shell-execute "explore" (dired-current-directory) "/e,/select,")))

  (defun my-dired-find-file ()
    ;; refer
    ;; consult-file-externally in consult
    ;; https://sakashushu.blog.ss-blog.jp/2014-04-29 "体当たり開始"
    (interactive)
    (if (eq (window-system) 'w32)
	(let* ((file-name (dired-get-file-for-visit))
	       (extension (file-name-extension file-name))
	       (assocfile (member extension '("MOV" "doc" "docx" "gif" "jpeg" "mp4" "pdf" "pptx" "xls" "xlsm" "xlsx"))))
	  (if assocfile
	      (cond ((or (string= (file-name-extension file-name) "MOV")
			 (string= (file-name-extension file-name) "mp4"))
		     ;; for windows 8.1
		     (shell-command-to-string (format "%s %s" "start" file-name)))
		    (t
		     (w32-shell-execute "open" file-name)))
	    (dired-find-file)))
      (dired-find-file)))

  )

(provide '.dired)
