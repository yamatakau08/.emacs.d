;; https://solist.work/blog/posts/google-translate/

;;(bind-key "C-c t" 'chromium-translate)

;;(define-key global-map (kbd "C-c t") 'chromium-translate)

(require 'url-util)

(defun chromium-translate ()
  "Open google translate with chromium."
  (interactive)
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
	(deactivate-mark)
	(if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
			  string)
	    (browse-url (concat "https://translate.google.com/?source=gtx#en/ja/"
				(url-hexify-string string)))
	  (browse-url (concat "https://translate.google.com/?source=gtx#ja/en/"
			      (url-hexify-string string)))))
    (let ((string (read-string "Google Translate: ")))
      (if (string-match
	   (format "\\`[%s]+\\'" "[:ascii:]")
	   string)
	  (browse-url
	   (concat "https://translate.google.com/?source=gtx#en/ja/" (url-hexify-string string)))
	(browse-url
	 (concat "https://translate.google.com/?source=gtx#ja/en/" (url-hexify-string string)))))))

(require 'google-translate)
(require 'google-translate-default-ui)

;(bind-key "C-c t" 'google-translate-auto)

(defvar toggle-translate-flg nil
  "Toggle flg.")

(defun toggle-translate ()
  "Toggle translate function."
  (interactive)
  (if toggle-translate-flg
      (progn
	(bind-key "C-c t" 'google-translate-auto)
	(setq toggle-translate-flg nil))
    (progn
      (bind-key "C-c t" 'chromium-translate)
      (setq toggle-translate-flg t))))

(defun google-translate-auto ()
  "Automatically recognize and translate Japanese and English."
  (interactive)
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
	(deactivate-mark)
	(if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
			  string)
	    (google-translate-translate
	     "en" "ja"
	     string)
	  (google-translate-translate
	   "ja" "en"
	   string)))
    (let ((string (read-string "Google Translate: ")))
      (if (string-match
	   (format "\\`[%s]+\\'" "[:ascii:]")
	   string)
	  (google-translate-translate
	   "en" "ja"
	   string)
	(google-translate-translate
	 "ja" "en"
	 string)))))

;; enable when you use popwin
;; (push "*Google Translate*" popwin:special-display-config)
