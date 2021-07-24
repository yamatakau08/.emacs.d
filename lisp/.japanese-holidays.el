(use-package japanese-holidays
  :ensure t
  :config
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(provide '.japanese-holidays)

