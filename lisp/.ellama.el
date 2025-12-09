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
    "Generate commit message based on diff in a new buffer."
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
      (let ((buf (get-buffer-create "*commit-message*")))
	(with-current-buffer buf
          (erase-buffer))
	(ellama-stream
	 (format ellama-generate-commit-message-template diff)
	 :buffer buf
	 :provider ellama-coding-provider
	 :on-done (lambda (_)
                    (pop-to-buffer (get-buffer "*commit-message*")))
	 :on-error (lambda (error-message)
                     (with-current-buffer (get-buffer "*commit-message*")
                       (erase-buffer)
                       (insert "Error occurred:\n\n")
                       (insert error-message))
                     (pop-to-buffer (get-buffer "*commit-message*")))))))

  )

(provide '.ellama)
