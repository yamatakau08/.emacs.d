(use-package plantuml-mode
  :ensure t

  :custom
  (plantuml-java-command "C:/Program Files/Android/Android Studio/jre/bin/java.exe") ; Windows 8.1
  (plantuml-jar-path "C:/Program Files/PlantUML/plantuml.1.2020.15.jar")
  (plantuml-default-exec-mode 'jar)

  )

(provide '.plantuml-mode)

;; ;; Sample jar configuration
;; (setq plantuml-jar-path "/path/to/your/copy/of/plantuml.jar")
;; (setq plantuml-default-exec-mode 'jar)

;; ;; Sample executable configuration
;; (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
;; (setq plantuml-default-exec-mode 'executable)

;; Windows 8.1
;; (my-plantuml-java "/C/Program Files/Android/Android Studio/jre/bin/java.exe")
;; Windows 10?
;; (defcustom my-plantuml-java "/C/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe"
;;   "java path"
;;   :group 'my-plantuml
;;   :type  'string)

;; (defcustom my-plantuml-plantuml "/C/Program Files/PlantUML/plantuml.1.2020.15.jar"
;;   "plantuml path"
;;   :group 'my-plantuml
;;   :type  'string)
