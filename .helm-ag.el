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

;;; for testing
;;; to suppress "File XXXX changed on disk. Read from disk?" dialog appears
;;; during executing C-n/C-p ... in helm-ag-this-file result buffer
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
  (with-output-to-temp-buffer "*My Backtrace*" (backtrace))
  (if (<= 2 (length args)) ; multiple argment
      (progn
	(setcar (nthcdr 1 args) t)
	args)
    ;; only filename argument
    (append args '(t))))

(advice-add 'find-file-noselect :filter-args #'my-find-file-noselect-filter-args)

;;; emacs-jp @kosh's sample code
;(defun find-file-noselect--filter-args (args)
;  ;; args=(filename &optional nowarn rawfile wildcards)
;  (with-output-to-temp-buffer "*My Backtrace*" (backtrace))
;  (if (<= 2 (length args))
;      (setf (nth 1 args) t)) ;; always nowarn=t
;  args)
;(advice-add 'find-file-noselect :filter-args
;            'find-file-noselect--filter-args)
