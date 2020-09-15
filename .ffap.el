(use-package ffap
  :config
  ;; modify ffap-string-at-point-mode-alist 'file
  ;; ffap treats ' ' and '　' zenkaku space as a part of the file path
  ;; to enalbe helm-find-files (C-x f) to open the above files
  (concat (cadr (assoc 'file ffap-string-at-point-mode-alist)) " 　")
)

;; test file path pattern
;; M-: (thing-at-point 'filename)
;; file path should be within "file path"
;; on windows
;;
;; "\\jps00004944\share_data\SQA1-HA\10_Test_Project_FY17\NewAudio\02_検証計画書\Schedule-NewAudio-Evaluation.pptx"
;; -> pass
;; "\\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　尊広\SQA2_山口　尊広_社員工数メトリクス表_v0080.xlsm"
;; -> fail
;; -> when ffap-string-at-point-mode-alist 'file is default value
;;    helm-find-files shows in minibuffer \\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019
;;    (ffap-string-at-point 'file) returns \\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　
;; -> add ' ' space and '　' zenkau space in ffap-string-at-point-mode-alist 'file
;;
;; on Mac
;; "~/Documents/QRチケット印刷 _ スワチケ2入口41段181番.pdf"
