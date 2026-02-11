(use-package llm ; shoud be llm, not llm-refactoring
  :ensure t
  :vc (:url "https://github.com/ahyatt/llm")

  :init
  (require 'llm-gemini))

(provide '.llm)
