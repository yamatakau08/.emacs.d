(use-package my-plantuml
  :load-path "my-plantuml"
  :custom
  ;; MSYS2
  ;;(my-plantuml-java "/C/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe")
  ;; Windows 10 /C/Program Files/Java/jdk-12.0.1/bin/java.exe
  ;; Windows 8  /C/Program Files/Android/Android Studio/jre/bin/java.exe
  ;;(my-plantuml-java "/C/Program Files/Android/Android Studio/jre/bin/java.exe") ; MSYS2

  ;; Cygwin
  (my-plantuml-java "c:/Program Files/Android/Android Studio/jre/bin/java.exe") ; availabe windows drive letter notation
  ;;(my-plantuml-plantuml "c:/Program Files/PlantUML/plantuml-1.2022.3.jar") ; availabe windows drive letter notation
  (my-plantuml-plantuml "c:/yama/bin/plantuml.jar") ; availabe windows drive letter notation on Laptop

  )

(provide '.my-plantuml)
