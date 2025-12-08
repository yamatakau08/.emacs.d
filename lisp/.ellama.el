(use-package ellama
  :ensure t

  :init
  ;; setup key bindings
  ;;(setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Japanese")
  (require 'llm-ollama)
  ;; normal conversation
  (setopt ellama-provider
	  (make-llm-ollama
	  :chat-model "gemma3:latest" ; gpt-oss-20b is too slow
	  :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; for coding
  )

(provide '.ellama)

