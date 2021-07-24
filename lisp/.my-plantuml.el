(use-package my-plantuml
  :load-path "my-plantuml"
  :custom
  ;;(my-plantuml-java "/C/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe")
  ;; Windows 10 /C/Program Files/Java/jdk-12.0.1/bin/java.exe
  ;; Windows 8  /C/Program Files/Android/Android Studio/jre/bin/java.exe
  (my-plantuml-java "/C/Program Files/Android/Android Studio/jre/bin/java.exe")
  )

(provide '.my-plantuml)

