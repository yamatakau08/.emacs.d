(use-package llm
  :unless (eq window-system 'w32)
  :ensure t

  :init
  (require 'llm-gemini))

(provide '.llm)
