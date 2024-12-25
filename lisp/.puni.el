;; https://github.com/AmaiKinono/puni?tab=readme-ov-file#quick-start
(use-package puni
  :ensure t
  
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(provide '.puni)
