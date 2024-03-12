(use-package embark
  :ensure t

  :custom
  ;; enable to execute M-x in minibuffer mode?
  ;; means e.g M-x embark-act is enable in consult-line narrowing result buffer is minibuffer.
  (enable-recursive-minibuffers t) ; conao3's advice

  :bind
  ;; referece is in oantolin is embark author
  ;; https://github.com/oantolin/emacs-config/blob/dfd0115ca82346e039191be385c2f6c3d3a0a61d/init.el
  (;;("C-." . embark-act)         ;; pick some comfortable binding, no effect!
   ;; note that "C-." is commented, because embark-act is prior.
   ("C-;"  . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   ("C-:" . embark-act-all)
   :map minibuffer-local-map ;; supported emacs-jp slack
   ("C-." . embark-act)         ;; pick some comfortable binding, no effect!
   ("C-c C-e" . embark-export)
   ("C-s" . embark-select)
   :map occur-mode-map
   ("C-c C-p" . occur-edit-mode) ;; to use the same binding C-c C-p on grep mode for editing
   :map dired-mode-map
   ("C-x x" . my-embark-open-externally)
   )

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun my-embark-open-externally ()
    (interactive)
    (embark-open-externally (dired-get-file-for-visit)))

  ;; the following code snippets from
  ;; https://github.com/oantolin/embark/issues/166#issuecomment-1058044854
  (defun +embark-mark (&optional unmark)
    (interactive)
    (unless (derived-mode-p #'embark-collect-mode)
      (error "Not in an Embark collect buffer"))
    (when-let (target (embark-target-collect-candidate))
      (pcase-let* ((`(,_type ,_cand ,beg . ,end) target)
                   (ov (seq-find (lambda (ov) (overlay-get ov '+embark-mark)) (overlays-at beg))))
	(unless (eq (not ov) unmark)
          (if ov
              (delete-overlay ov)
            (unless (facep 'dired-marked)
              (require 'dired))
            (setq ov (make-overlay beg end))
            (overlay-put ov '+embark-mark t)
            (overlay-put ov 'face 'dired-marked)))))
    ;; (call-interactively #'next-line)
    (call-interactively #'forward-button)
    )

  (defun +embark-unmark ()
    (interactive)
    (+embark-mark t))

  (defun +embark-marked-candidates ()
    (when-let (ovs (and (derived-mode-p #'embark-collect-mode)
			(nreverse (seq-filter (lambda (ov) (overlay-get ov '+embark-mark))
                                              (overlays-in (point-min) (point-max))))))
      (cons embark--type
            (save-excursion
              (mapcar (lambda (ov)
			(goto-char (overlay-start ov))
			(cadr (embark-target-collect-candidate)))
                      ovs)))))

  (add-to-list 'embark-candidate-collectors #'+embark-marked-candidates)
  (define-key embark-collect-mode-map "m" #'+embark-mark)
  (define-key embark-collect-mode-map "u" #'+embark-unmark)

  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide '.embark)
