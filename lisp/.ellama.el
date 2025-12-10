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

  :config
  (defun my-ellama-generate-commit-message ()
    "Show the commit message genereated by ellama-generate-commit-message in the buffer *ellama-commit-message*"
    (interactive)
    (when-let* ((default-directory
		 (if (string= ".git"
                              (car (reverse
                                    (cl-remove
                                     ""
                                     (file-name-split default-directory)
                                     :test #'string=))))
                     (file-name-parent-directory default-directory)
                   default-directory))
		(diff (or (ellama--diff-cached)(ellama--diff))))
      (let* ((buf-name "*ellama-commit-message*")
	     (buf (get-buffer-create buf-name)))
	(pop-to-buffer buf)
	(with-current-buffer buf
	  (erase-buffer)
	  (ellama-stream
	   (format ellama-generate-commit-message-template diff)
	   :provider ellama-coding-provider)))))

  )

(provide '.ellama)
