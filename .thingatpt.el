;;; customize thingatpt.el to get the file path in which has ' ','　' and '\' character

;;; https://emacs.stackexchange.com/a/33441
(with-eval-after-load "thingatpt"
  (setq thing-at-point-file-name-chars (concat thing-at-point-file-name-chars " 　\\")) ; add ' ' space, '　' zenkaku space, '\\' to get windows file path
  )

;;; test file path pattern
;;; M-: (thing-at-point 'filename)
;;; file path should be within "file path"
;;; on windows
;;; "\\jps00004944\share_data\SQA1-HA\10_Test_Project_FY17\NewAudio\02_検証計画書\Schedule-NewAudio-Evaluation.pptx"
;;; "\\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　尊広\SQA2_山口　尊広_社員工数メトリクス表_v0080.xlsm"
;;;
;;; on my mac
;;; "~/Documents/QRチケット印刷 _ スワチケ2入口41段181番.pdf"
;;: if path has ' ' space or '　' zenkaku space, (things-at-point 'filename) can return path correctly,
