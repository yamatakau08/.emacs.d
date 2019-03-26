;;; https://company-mode.github.io/
;;; enable company mode in all buffer
;(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode)

;;; refer https://qiita.com/kod314/items/3a31719db27a166d2ec1#%E8%A8%AD%E5%AE%9A%E3%81%97%E3%81%9F%E3%81%93%E3%81%A8
;;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
