(use-package my-plantuml
  :load-path "my-plantuml"
  :custom
  ;; MSYS2
  ;;(my-plantuml-java "/C/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe")
  ;; Windows 10 /C/Program Files/Java/jdk-12.0.1/bin/java.exe
  ;; Windows 8  /C/Program Files/Android/Android Studio/jre/bin/java.exe
  ;;(my-plantuml-java "/C/Program Files/Android/Android Studio/jre/bin/java.exe") ; MSYS2

  ;; Cygwin
  (my-plantuml-java "/cygdrive/c/Program Files/Android/Android Studio/jre/bin/java.exe") ; need cygwin path notation for fish shell to understand
  (my-plantuml-plantuml "c:/yama/bin/plantuml.jar") ; should be windows drive letter notation, because java is windows native program understand drive letter notation not cygwin path notation
  )

(provide '.my-plantuml)
