(use-package embark
  :ensure t

  :custom
  ;; enable to execute M-x in minibuffer mode?
  ;; means e.g M-x embark-act is enable in consult-line narrowing result buffer is minibuffer.
  (enable-recursive-minibuffers t) ; conao3's advice

  :bind
  (;;("C-." . embark-act)         ;; pick some comfortable binding, no effect!
   ;; note that "C-." is commented, because embark-act is prior.
   ("C-;"  . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   :map minibuffer-local-map ;; supported emacs-jp slack
   ("C-." . embark-act)         ;; pick some comfortable binding, no effect!
   ("C-c C-e" . embark-export))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
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
