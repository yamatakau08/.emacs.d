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

(defun helm-ag--find-file-action--filter-args (args)
  ;;(print args) ; for debug
  (if (eq (nth 1 args) 'find-file)
      (setf (nth 1 args) (lambda (filename) (switch-to-buffer (find-file-noselect filename t))))
      ;(setf (nth 1 args) 'helm-ag--find-file-action-find-file)
    )
  ;;(print args) ; for debug
  args)

(advice-add #'helm-ag--find-file-action :filter-args
	    #'helm-ag--find-file-action--filter-args)

;; To pass ag option, such as: --elisp --depth 0 in helm-ag
;; refer https://github.com/syohex/emacs-helm-ag/blob/master/README.md#use-long-option
;; --ignore=pattern is oK, --ignore pattern is not oK
;; in minibuffer helm-ag prompt, input rg option following
;; Pattern: --elisp --depth=0 pattern
;; --elisp, you can find file types ag --list-file-types
;; > ag --list-file-types
;; ...
;;  --batch
;;     .bat  .cmd
;;  --elisp
;;      .el
;;  --log
;;      .log
;;  --org
;;      .org
;;  --ruby
;;      .rb  .rhtml  .rjs  .rxml  .erb  .rake  .spec
;;  --shell
;;      .sh  .bash  .csh  .tcsh  .ksh  .zsh  .fish

;; ag additional option
;(custom-set-variables '(helm-ag-command-option "--depth 0"))

;; i tried to pass ag option (helm-ag-command-option "--depth 0") until I don't know how to pass ag option to helm-ag
;(defun my-helm-do-ag (&optional basedir targets)
;  (interactive)
;  (require 'helm-mode)
;  (helm-ag--init-state)
;  (let* ((helm-ag-command-option "--depth 0") ; for test
;	 (helm-ag--default-directory (or basedir default-directory))
;         (helm-ag--default-target (cond (targets targets)
;                                        ((and (helm-ag--windows-p) basedir) (list basedir))
;                                        (t
;                                         (when (and (not basedir) (not helm-ag--buffer-search))
;                                           (helm-read-file-name
;                                            "Search in file(s): "
;                                            :default default-directory
;                                            :marked-candidates t :must-match t)))))
;         (helm-do-ag--extensions (when helm-ag--default-target
;                                   (helm-ag--do-ag-searched-extensions)))
;         (one-directory-p (helm-do-ag--target-one-directory-p
;                           helm-ag--default-target)))
;    (helm-ag--set-do-ag-option)
;    (helm-ag--set-command-features)
;    (helm-ag--save-current-context)
;    (helm-attrset 'search-this-file
;                  (and (= (length helm-ag--default-target) 1)
;                       (not (file-directory-p (car helm-ag--default-target)))
;                       (car helm-ag--default-target))
;                  helm-source-do-ag)
;    (if (or (helm-ag--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
;        (helm-do-ag--helm)
;      (let* ((helm-ag--default-directory
;              (file-name-as-directory (car helm-ag--default-target)))
;             (helm-ag--default-target nil))
;        (helm-do-ag--helm)))))


;; don't use this settings
;(when (featurep 'helm-ag)
;  ;; replace C-s/C-r to helm-ag-this-file
;  ;; firstly I replace C-s/C-r with helm-ag-this-file.
;  ;; but it can't be used in buffers e.g. *GNU Emacs* is not file buffers
;  ;; helm-ag-this-file: Wrong type argument: stringp, nil
;  (define-key global-map (kbd "C-s") 'helm-ag-this-file)
;  (define-key global-map (kbd "C-r") 'helm-ag-this-file)
;  )

;; if rg exist, set rg as helm-ag-base-command
(setq rg-command nil) ; if rg command doesn't exist, set "".
;(setq rg-command "/mingw64/bin/rg.exe") ; need .exe on windows environment
;; When use rg, can't follow the pattern while move the cursor line in search result buffer.
;; I don't use rg
(when rg-command
  (when (file-exists-p rg-command)
    (custom-set-variables
     '(helm-ag-base-command "rg --no-heading"))))
