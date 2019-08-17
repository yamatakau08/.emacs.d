(autoload 'counsel-ag "counsel.el")

;(setq counsel-ag-base-command
;  (if (memq system-type '(ms-dos windows-nt))
;      "ag --vimgrep %s"
;    "ag --nocolor --nogroup %s"))

; counsel-ag-base-command
(defun counsel-ag-this-file ()
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (helm-ag--default-directory default-directory))
    (helm-ag--query)
    (helm-ag--set-command-features)
    (helm-attrset 'search-this-file (file-relative-name (buffer-file-name))
                  helm-ag-source)
    (helm-attrset 'name (format "Search at %s" filename) helm-ag-source)
    (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
          :history 'helm-ag--helm-history)))

(defun counsel-tako ()
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (read-directory-name (concat
                                    (car (split-string counsel-ag-command))
                                    " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (read-from-minibuffer (format
                                     "%s args: "
                                     (car (split-string counsel-ag-command)))))))
  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
  (let ((default-directory (or initial-directory
                               (counsel--git-root)
                               default-directory)))
    (ivy-read (or ag-prompt
                  (concat (car (split-string counsel-ag-command)) ": "))
              #'counsel-ag-function
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller (or caller 'counsel-ag))))

;(defun helm-ag-this-file ()
;  (interactive)
;  (helm-ag--init-state)
;  (let ((filename (file-name-nondirectory (buffer-file-name)))
;        (helm-ag--default-directory default-directory))
;    (helm-ag--query)
;    (helm-ag--set-command-features)
;    (helm-attrset 'search-this-file (file-relative-name (buffer-file-name))
;                  helm-ag-source)
;    (helm-attrset 'name (format "Search at %s" filename) helm-ag-source)
;    (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
;          :history 'helm-ag--helm-history)))

;(cl-defun counsel-ag-this-file (&optional initial-input initial-directory extra-ag-args ag-prompt
;                      &key caller)
;  "Grep for a string in the current directory using ag.
;INITIAL-INPUT can be given as the initial minibuffer input.
;INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
;EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
;AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
;CALLER is passed to `ivy-read'."
;  (interactive)
;  (setq counsel-ag-command counsel-ag-base-command)
;  (setq counsel--regex-look-around counsel--grep-tool-look-around)
;  (counsel-require-program counsel-ag-command)
;  (when current-prefix-arg
;    (setq initial-directory
;          (or initial-directory
;              (read-directory-name (concat
;                                    (car (split-string counsel-ag-command))
;                                    " in directory: "))))
;    (setq extra-ag-args
;          (or extra-ag-args
;              (read-from-minibuffer (format
;                                     "%s args: "
;                                     (car (split-string counsel-ag-command)))))))
;  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
;  (let ((default-directory (or initial-directory
;                               (counsel--git-root)
;                               default-directory)))
;    (ivy-read (or ag-prompt
;                  (concat (car (split-string counsel-ag-command)) ": "))
;              #'counsel-ag-function
;              :initial-input initial-input
;              :dynamic-collection t
;              :keymap counsel-ag-map
;              :history 'counsel-git-grep-history
;              :action #'counsel-git-grep-action
;              :unwind (lambda ()
;                        (counsel-delete-process)
;                        (swiper--cleanup))
;              :caller (or caller 'counsel-ag))))

