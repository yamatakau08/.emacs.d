;;;
(use-package openwith
  :ensure t
  )

;;; refer https://github.com/emacsmirror/openwith/blob/master/README.txt
(when (require 'openwith nil 'noerror)
  (cond
   ((eq system-type 'windows-nt)
    (setq openwith-associations
	  '("\\.pptx" nil (file)) ; nil: Since "open" in specified in openwith.el selects application associated with file suffix, we don't need to specify application program.
	  (list (openwith-make-extension-regexp
		 '("xlsx" "xlsm"))
		nil
		'(file))))
   ((eq system-type 'darwin)
    (setq openwith-associations
	  (list
	   '("\\.pdf"     "open" (file))
	   '("\\.numbers" "open" (file))))))
  (openwith-mode 1))

;;; 'file in ffap-string-at-point-mode-alist
;; ' ' and '　' zenkau space for path has zenkaku space
;; file path should be within "file path" or 'file path'
;; callf: refer https://qiita.com/kawabata@github/items/9a1a1e211c57a56578d8#%E6%B1%8E%E5%A4%89%E6%95%B0%E3%82%92%E6%89%B1%E3%81%86%E3%83%9E%E3%82%AF%E3%83%AD%E3%81%A8%E3%81%9D%E3%81%AE%E4%BE%8B
;; test path pattern
;; "~/Documents/QRチケット印刷 _ スワチケ2入口41段181番.pdf"
(with-eval-after-load "ffap"
  ;; (callf concat (cdr (assoc 'file ffap-string-at-point-mode-alist)) " 　") ; fail

  (setq ffap-string-at-point-mode-alist
	'(
	  ;; The default, used when the `major-mode' is not found.
	  ;; Slightly controversial decisions:
	  ;; * strip trailing "@" and ":"
	  ;; * no commas (good for latex)
	  (file "--:\\\\${}+<>@-Z_[:alpha:]~*? 　" "<@" "@>;.,!:") ; add ' ' '　'
	  ;; An url, or maybe an email/news message-id:
	  (url "--:=&?$+@-Z_[:alpha:]~#,%;*()!'" "^[0-9a-zA-Z]" ":;.,!?")
	  ;; Find a string that does *not* contain a colon:
	  (nocolon "--9$+<>@-Z_[:alpha:]~" "<@" "@>;.,!?")
	  ;; A machine:
	  (machine "-[:alnum:]." "" ".")
	  ;; Mathematica paths: allow backquotes
	  (math-mode ",-:$+<>@-Z_[:lower:]~`" "<" "@>;.,!?`:")
	  ;; (La)TeX: don't allow braces
	  (latex-mode "--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:")
	  (tex-mode "--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:")
	  ))
)

;;; execute C-x f with helm on string "\\jps00004944\share_data\SQA1-HA\10_Test_Project_FY17\NewAudio\02_検証計画書\Schedule-NewAudio-Evaluation.pptx"
;;; should be added "\\" in thing-at-point-file-name-chars variable
;;; C-x f finally invoke C-x C-f find-files, then openwith-open-windows is called
;;;
;;: if path has ' ' space or '　' zenkaku space, (things-at-point 'filename) can return path correctly,
;;; "\\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　尊広\SQA2_山口　尊広_社員工数メトリクス表_v0080.xlsm"
;;; but C-x f with helm shows \\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019 in mini-buffer
;;; (ffap-string-at-point 'file) returns \\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　
(defun openwith-open-windows-filter-args (arg)
  ;; this filter advice function
  ;; arg is list ("//jps00004944/share_data/SQA1-HA/10_Test_Project_FY17/NewAudio/02_検証計画書/Schedule-NewAudio-Evaluation.pptx")
  ;; replace car of arg with
  ;;; "\\\\jps00004944\\share_data\\SQA1-HA\\10_Test_Project_FY17\\NewAudio\\02_検証計画書\\Schedule-NewAudio-Evaluation.pptx"
  (list (my-app-open-file-path2explore (car arg)))
  )

(advice-add #'openwith-open-windows :filter-args
	    #'openwith-open-windows-filter-args)
