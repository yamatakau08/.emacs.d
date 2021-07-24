(require 'wgrep)
;(require 'counsel)
;; assign "C-x f" likely helm-find-files
(global-set-key (kbd "C-x f") 'counsel-find-file)

;; disable counsel rebinding "C-x C-f" find-file
;; https://qiita.com/takaxp/items/2fde2c119e419713342b#counsel-find-file-%E3%82%92%E4%BD%BF%E3%82%8F%E3%81%AA%E3%81%84
;; with-eval-after-load https://stackoverflow.com/questions/21880139/what-is-with-eval-after-load-in-emacs-lisp
(with-eval-after-load "counsel"
  (defun my-disable-counsel-find-file (&rest args)
    "Disable `counsel-find-file' and use the original `find-file' with ARGS."
    (let ((completing-read-function #'completing-read-default)
          (completion-in-region-function #'completion--in-region))
      (apply #'read-file-name-default args)))
  (setq read-file-name-function #'my-disable-counsel-find-file)
  (define-key counsel-mode-map [remap find-file] nil))

;; for reference to make counsel-ag-this-file function
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

;; counsel-grep でも代用可能
;; counsel-grep-or-swiper を使えばファイルでないbufferも対象に
(defalias 'counse-ag-this-file 'counsel-grep-or-swiper)

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

;;; To edit the search results of counsel-grep
;;; need to pacakge wgrep wgrep-ag
;;; refer https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
(defun my-ivy-occur ()
  "Stop completion and put the current candidates into a new buffer.

The new buffer remembers current action(s).

While in the *ivy-occur* buffer, selecting a candidate with RET or
a mouse click will call the appropriate action for that candidate.

There is no limit on the number of *ivy-occur* buffers."
  (interactive)
  (if (not (window-minibuffer-p))
      (user-error "No completion session is active")
    (let* ((caller (ivy-state-caller ivy-last))
           (occur-fn (plist-get ivy--occurs-list caller))
           (buffer
            (generate-new-buffer
             (format "*ivy-occur%s \"%s\"*"
                     (if caller
                         (concat " " (prin1-to-string caller))
                       "")
                     ivy-text))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if occur-fn
              (funcall occur-fn)
            (ivy-occur-mode)
            (insert (format "%d candidates:\n" (length ivy--old-cands)))
            (read-only-mode)
            (ivy--occur-insert-lines
             ivy--old-cands)))
        (setf (ivy-state-text ivy-last) ivy-text)
        (setq ivy-occur-last ivy-last))
      (ivy-exit-with-action (lambda (_) (funcall ivy-wgrep-change-to-wgrep-mode))))))

(defun my-ivy-occur-edit ()
  (interactive) ; need to interactive, to avoid the following error
  ;; (wrong-type-argument commandp my-ivy-occur-edit)
  ;; call-interactively(my-ivy-occur-edit nil nil)
  ;; command-execute(my-ivy-occur-edit)
  (ivy-occur)
  ;; Since "ivy-exit-with-action" function exit in "iv-occur" function,
  ;; ivy-wgrep-change-to-wgre-mode can't be executed
  (ivy-wgrep-change-to-wgrep-mode))

;; counsel-ag-map
;; ivy buffer ag のpromptが出ている時に有効なKey map
;; https://tomoya.hatenadiary.org/entry/20090415/1239809615
(with-eval-after-load "ivy"
  (define-key ivy-minibuffer-map (kbd "C-c C-e") 'my-ivy-occur-edit))

;; C-x f doesn't work in case of URL

