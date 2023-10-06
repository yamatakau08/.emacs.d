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
  ;(dired-dwim-target t)
  ;; for easy to copy the file in dired A to dired B,
  ;; In case dired A is already opened and open dired B,
  ;; in dired B buffer execute copy or move the file, dired A path is automaticaly put as destination directory.
  (dired-dwim-target 'dired-dwim-target-recent)

  ;;
  (dired-guess-shell-alist-user
   (list
    (list "\\.t\\(ar\\.\\)?gz\\'"
          '(if dired-guess-shell-gnutar
               (concat dired-guess-shell-gnutar " zxvfk")
             (concat "gunzip -qc * | tar xvf -"))
          ;; Extract files into a separate subdirectory
          '(if dired-guess-shell-gnutar
               (concat "mkdir " (file-name-sans-extension file)
                       "; " dired-guess-shell-gnutar " -C "
                       (file-name-sans-extension file) " -zxvf")
             (concat "mkdir " (file-name-sans-extension file)
                     "; gunzip -qc * | tar -C "
                     (file-name-sans-extension file) " -xvf -"))
          ;; Optional decompression.
          '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q" ""))
          ;; List archive contents.
          '(if dired-guess-shell-gnutar
               (concat dired-guess-shell-gnutar " ztvf")
             (concat "gunzip -qc * | tar tvf -")))))

  :bind
  (:map dired-mode-map
	("C-l"      . my-dired-open-directory)
	("C-j"      . my-dired-find-file)
	("<return>" . my-dired-find-file)
	("N"        . my-dired-ffmpeg-comp))

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

  (defun camerarollopen ()
    "open camearrollcopy directory in dired buffer
if dired-dwim-target is set t, dired guess a default target directory.
it easy to select the file to copy into target directory."
    (interactive)
    (if onedrive-cameraroll-folder
        (dired-other-window onedrive-cameraroll-folder)
      (dired-other-window "c:/Users/0000910700/Pictures/Camera Roll/")))

  (defun my-dired-ffmpeg-comp ()
    (interactive)
    (cond
     ((eq (window-system) 'w32)
      (let* ((file-path (dired-get-file-for-visit)))
	(ffmpegcomp file-path)))
     (t
      (message (concat (window-system) "is not supported")))))

  )


(provide '.dired)
