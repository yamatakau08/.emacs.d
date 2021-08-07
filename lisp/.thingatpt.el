(use-package thingatpt
  :config
  ;; add ' ' space and '　' zenkaku space, '\\' to get windows file path
  ;; Since :custom section doesn't reflect the setting, use :config section and setq function.
  ;; When the variable is set by defvar function, should use setq function
  (setq thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,: 　\\")

  (defun xmy-thing-at-point-filename ()
    "Even if cursor is on '\"' or '<' character is preceding file name, returns filename"
    (let* ((byte (get-byte))
	   (char (format "%c" byte))) ; alternative (char (string (char-after)))
      (save-excursion
	(if (or (equal char "\"")
		(equal char "<"))
	    (forward-char))
	(thing-at-point 'filename t))))

  (defun my-thing-at-point-filename ()
    "Even if cursor is on '\"' or '<' character is preceding file name, returns filename"
    (save-excursion
      (skip-chars-forward "\"<")
      (thing-at-point 'filename t)))

  )

;; thing-at-point-file-name-chars
;; test file path pattern
;; M-: (thing-at-point 'filename)
;; file path should be within "file path"
;; on windows
;; "\\jps00004944\share_data\SQA1-HA\10_Test_Project_FY17\NewAudio\02_検証計画書\Schedule-NewAudio-Evaluation.pptx"
;; "\\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　尊広\SQA2_山口　尊広_社員工数メトリクス表_v0080.xlsm"
;; fail when point is on '<'
;; <\\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　尊広\SQA2_山口　尊広_社員工数メトリクス表_v0080.xlsm>

;; on my mac
;; "~/Documents/QRチケット印刷 _ スワチケ2入口41段181番.pdf"
;: if path has ' ' space or '　' zenkaku space, (things-at-point 'filename) can return path correctly,

(provide '.thingatpt)
