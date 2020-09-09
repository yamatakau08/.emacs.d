;; on Mac
;; say -v '?' | grep en_US
;; Alex                en_US    # Most people recognize me by my voice.
;; Fred                en_US    # I sure like being inside this fancy computer
;; Samantha            en_US    # Hello, my name is Samantha. I am an American-English voice.
;; Victoria            en_US    # Isn't it nice to have a computer that will talk to you?

(defvar say-voice "Samantha")

;; https://multilingual-jonny.com/reading-speed-wpm/
;; 自然な会話スピード（速め）: 160wpm〜200wpm
(defvar say-rate 160)

(defun say-eword ()
  "say english word which is at point"
  (interactive)
  (let* ((word (thing-at-point 'word))
	 (cmd (format "say -v \"%s\" \"%s\"" say-voice word)))
    (shell-command-to-string cmd)))

(defun say-eline ()
  "say english line which is at point"
  (interactive)
  (let* ((line (thing-at-point 'line))
	 ;; split-string ":" is to get the english phrase of "Eced result"
	 (cmd (format "say -v \"%s\" -r %s \"%s\"" say-voice say-rate (cadr (split-string line ":")))))
    (shell-command-to-string cmd)))

(provide 'my-say)
