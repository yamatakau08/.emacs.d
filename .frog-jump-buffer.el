(use-package frog-jump-buffer
  :ensure t

  :config
  (custom-set-variables
   '(frog-jump-buffer-default-filter 'frog-jump-buffer-filter-recentf)
   '(frog-jump-buffer-max-buffers 20))

  ;; redfine original function
  (defun frog-jump-buffer-find-or-create-buffer (res)
    "Switch to buffer, or if closed, find and create it first."
    (let ((buffer (if frog-jump-buffer-include-virtual-buffers
		      (find-file (assoc-default res (frog-jump-buffer-recentf-buffers)))
                    res)))
      (if frog-jump-buffer-target-other-window
          ;;(switch-to-buffer-other-window buffer) ; original
          (switch-to-buffer-other-window nil) ; my modification
	(switch-to-buffer buffer))))

  :bind (("C-x b" . frog-jump-buffer)
	 ("C-x c" . frog-jump-buffer-other-window)))

;; need to modify to sort filename (nreverse buffers)
;(defun frog-jump-buffer-recentf-buffers ()
;  "Adapted from `ivy--virtual-buffers'."
;  (unless recentf-mode
;    (recentf-mode 1))
;  (let (buffers)
;    (dolist (head recentf-list)
;      (let* ((file-name (if (stringp head) head (cdr head)))
;             (name (file-name-nondirectory file-name)))
;        (when (equal name "")
;          (setq name
;                (if (consp head)
;                    (car head)
;                  (file-name-nondirectory (directory-file-name file-name)))))
;        (unless (or (equal name "")
;                    (assoc name buffers))
;          (push (cons (copy-sequence name) file-name) buffers))))
;    (when buffers
;      (message "[debug] frog-jump-buffer-recentf-buffers buffers: %s" buffers)
;      (nreverse buffers))))
;
;((window.el . c:/winbin/emacs-28.0.50-snapshot-2020-07-05-x86_64/share/emacs/28.0.50/lisp/window.el) (my-skips.el . c:/yama/.emacs.d/my-skips.el) (.package.el . c:/yama/.emacs.d/.package.el) (EBISU_Update.org . c:/yama/confluence/EBISU_Update.org) (.howm-keys . c:/yama/.howm-keys) (.helm-ag.el . c:/yama/.emacs.d/.helm-ag.el) (.org.el . c:/yama/.emacs.d/.org.el) (helm-buffers.el . c:/yama/.emacs.d/elpa/helm-20200808.430/helm-buffers.el) (company-clang.el . c:/yama/.emacs.d/elpa/company-20200807.48/company-clang.el) (helm-for-files.el . c:/yama/.emacs.d/elpa/helm-20200808.430/helm-for-files.el) (helm.el . c:/yama/.emacs.d/elpa/helm-core-20200803.1032/helm.el) (helm-posframe-autoloads.el . c:/yama/.emacs.d/elpa/helm-posframe-20200512.1146/helm-posframe-autoloads.el) (ad_org-clock-report.el . c:/yama/.emacs.d/ad_org-clock-report.el) (2006-05-16-192006.txt . c:/yama/howm/2006/05/2006-05-16-192006.txt) (.helm.el . c:/yama/.emacs.d/.helm.el) (.frog-jump-buffer.el . c:/yama/.emacs.d/.frog-jump-buffer.el) (helm-multi-match.el . c:/yama/.emacs.d/elpa/helm-core-20200803.1032/helm-multi-match.el) (init.el . c:/yama/.emacs.d/init.el) (helm-occur.el . c:/yama/.emacs.d/elpa/helm-20200808.430/helm-occur.el) (frog-jump-buffer.el . c:/yama/.emacs.d/elpa/frog-jump-buffer-20200114.1826/frog-jump-buffer.el))
