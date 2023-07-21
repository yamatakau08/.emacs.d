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
  :custom
  (dired-dwim-target t)

  :bind
  (:map dired-mode-map
	("C-l"      . my-dired-open-directory)
	("C-j"      . my-dired-find-file)
	("<return>" . my-dired-find-file))

  :config
  ;; original dired-get-marked-files function returns the file path under the point when there is no marked files.
  (defun my-dired-get-marked-files ()
    (when (> (string-to-number (dired-number-of-marked-files)) 0)
      (dired-get-marked-files)))

  (defun my-dired-open-directory ()
    "Open directory with Windows explore"
    (interactive)
    (cond
     ((eq (window-system) 'w32)
      (let* ((directory-path (dired-current-directory)))
	(my-w32-open-file directory-path t) ; t:  open directory by explore
	))
     (t
      (dired-find-file))))

  (defun my-dired-find-file ()
    ;; refer
    ;; consult-file-externally in consult
    ;; https://sakashushu.blog.ss-blog.jp/2014-04-29 "体当たり開始"
    (interactive)
    (cond
     ((eq (window-system) 'w32)
      (let* ((file-path (dired-get-file-for-visit)))
	(my-w32-open-file file-path)))
     (t
      (dired-find-file))))

  (defun camerarollcopy ()
    "open camearrollcopy directory in dired buffer
if dired-dwim-target is set t, dired guess a default target directory.
it easy to select the file to copy into target directory."
    (interactive)
    (dired-other-window "c:/Users/0000910700/Pictures/Camera Roll/"))

  )


(provide '.dired)
