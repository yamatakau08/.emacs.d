;;; company official site: https://company-mode.github.io/

;;; to suppress error when emacs launch
;;; error: eval-buffer: Symbol’s value as variable is void: company-active-map
;;; comment the following to use use-package
;(require 'company)

;;; to https://kumaroot.readthedocs.io/ja/latest/emacs-use-package.html#id2
(use-package company
  :ensure t
  :config
  ;; enable company mode in all buffer
  ;; (global-company-mode t)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; to use C-n C-p to select next/prev candidate
  ;; refer https://qiita.com/kod314/items/3a31719db27a166d2ec1#%E8%A8%AD%E5%AE%9A%E3%81%97%E3%81%9F%E3%81%93%E3%81%A8
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))
