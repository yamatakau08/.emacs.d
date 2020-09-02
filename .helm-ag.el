(use-package helm-ag
  :ensure t

  :init
  ;; on mac
  ;; $ brew install ag, ag is installed in /usr/local/bin
  ;; unfortunately, exec-path doesn't include /usr/local/bin even if that path is set in .bash_profile.
  ;; tentative add /usr/local/bin in exec-path
  (if (eq system-type 'darwin)
      (add-to-list 'exec-path "/usr/local/bin"))

  :config
  (custom-set-variables
   '(helm-ag-base-command
     ;; add --hidden option
     (if (helm-ag--windows-p)
	 "ag --vimgrep --hidden"
       "ag --nocolor --nogroup --hidden")))

  :bind
  ;; don't use this settings
  ;; replace C-s/C-r to helm-ag-this-file
  ;; firstly I replace C-s/C-r with helm-ag-this-file.
  ;; it's not availabe in buffers e.g. *GNU Emacs*,is available ony file buffers
  ;;(("C-s" . helm-ag-this-file)
  ;; ("C-r" . helm-ag-this-file))
  )

;;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
;;; in helm-ag candidate buffer, selected search result with ctr-n or ctr-p
;;; other window follow that action and shows the part selected in candidate buffer
;;(setq helm-follow-mode-persistent t) ; since this setting is for helm itself, move to .helm.el

;;; to suppress "File XXX changed on disk. Read from disk?" dialog
;;; while helm-ag-this file XXX file is updating, likely log file

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
  (if (eq (nth 1 args) 'find-file)
      (setf (nth 1 args) (lambda (filename) (switch-to-buffer (find-file-noselect filename t))))
    ;;(setf (nth 1 args) 'helm-ag--find-file-action-find-file)
    )
  args)

(advice-add #'helm-ag--find-file-action :filter-args
	    #'helm-ag--find-file-action--filter-args)

;; To pass ag option, such as: --elisp --depth 0 in helm-ag
;; refer https://github.com/syohex/emacs-helm-ag/blob/master/README.md#use-long-option
;; --ignore=pattern is pass, --ignore pattern is fail
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

;; if rg exist, set rg as helm-ag-base-command
;;(setq rg-command "/mingw64/bin/rg.exe") ; need .exe on windows environment

(when (boundp 'rg-command)
  (if (file-executable-p rg-command)
      (custom-set-variables
       ;; http://emacs.rubikitch.com/helm-ag/ <2016-10-05 Wed>ripgrep対応
       ;;'(helm-ag-base-command "rg --no-heading")
       '(helm-ag-base-command "rg --vimgrep --no-heading")) ; to follow the search pattern in other window, need "--vimgrep"
    ))
