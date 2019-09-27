(use-package helm-ag
  :ensure t)

;;; to install "ag"
;;; $ brew install ag, ag is installed in /usr/local/bin on mac
;;; unfortunately, exec-path doesn't include /usr/local/bin even if that path is set in .bash_profile.
;;; tentative add /usr/local/bin in exec-path
(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/usr/local/bin"))

;;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
;;; in helm-ag candidate buffer, selected search result with ctr-n or ctr-p
;;; other window follow that action and shows the part selected in candidate buffer
;;(setq helm-follow-mode-persistent t) ; since this setting is for helm itself, move to .helm.el

;;; to suppress "File XXX changed on disk. Read from disk?" dialog
;;; while helm-ag-this file XXX file is updating, likely log file
;; for testing
(defun org-find-file-noselect (filename &optional nowarn rawfile wildcards)
  (message "filename:%s, nowarn:%s, rawfile:%s, wildcards:%s" filename nowarn rawfile wildcards))
(defun my-find-file-noselect (filename &optional nowarn rawfile wildcards)
  (message "filename:%s, nowarn:%s, rawfile:%s, wildcards:%s" filename nowarn rawfile wildcards))

;; "&rest" keyword of args is unnecessary
;; (defun my-find-file-noselect-filter-args (&rest args) first definition
;; when "&rest" is append, arg content is ((args))
(defun my-find-file-noselect-filter-args (args)
  ;; args=(filename &optional nowarn rawfile wildcards)
  ;; always nowarn=t
  (with-output-to-temp-buffer "*My Backtrace*" (backtrace)) ; to check where the function is called
  (if (<= 2 (length args)) ; multiple argment
      (progn
	(setcar (nthcdr 1 args) t)
	args)
    ;; only filename argument
    (append args '(t))))

;(advice-add 'find-file-noselect :filter-args #'my-find-file-noselect-filter-args)

;;; emacs-jp @kosh's sample code
;;; https://emacs-jp.slack.com/archives/C6T2T9H4G/p1568479222029200
;(defun find-file-noselect--filter-args (args)
;  ;; args=(filename &optional nowarn rawfile wildcards)
;  (with-output-to-temp-buffer "*My Backtrace*" (backtrace))
;  (if (<= 2 (length args))
;      (setf (nth 1 args) t)) ;; always nowarn=t
;  args)
;(advice-add 'find-file-noselect :filter-args
;            #'find-file-noselect--filter-args)

;;; replace function of "find-file" for helm-ag--find-file-action
;;; when calling helm-ag-this-file and result buffer with search string
;;; during target-file is always updating
(defun helm-ag--find-file-action-find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)File name Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename t nil wildcards))) ; force set to 2'nd arg t of find-file-noselect
    (if (listp value)
	(mapcar 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))))

(defun helm-ag--find-file-action--filter-args (args)
  ;(print args) ; for debug
  (if (eq (nth 1 args) 'find-file)
      (setf (nth 1 args) (lambda (filename) (switch-to-buffer (find-file-noselect filename t))))
      ;(setf (nth 1 args) 'helm-ag--find-file-action-find-file)
    )
  ;(print args) ; for debug
  args)

(advice-add 'helm-ag--find-file-action :filter-args
	    #'helm-ag--find-file-action--filter-args)
