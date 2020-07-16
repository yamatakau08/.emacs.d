;;; my-plantuml.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <yamatakau08@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defgroup my-plantuml nil
  "PlantUML execution"
  :group 'tools)

(defcustom my-plantuml-java "/C/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe"
  "java path"
  :group 'my-plantuml
  :type  'string)

(defcustom my-plantuml-plantuml "/C/Program Files/PlantUML/plantuml.1.2020.15.jar"
  "plantuml path"
  :group 'my-plantuml
  :type  'string)

;; public
(defun my-plantuml-exec ()
  (interactive)
  (if (buffer-file-name)
      (if (string= "puml" (file-name-extension (buffer-file-name)))
	  (my-plantuml--exec (buffer-file-name))
	(message "[my-plantuml] -exec cmd: %s" (buffer-file-name))
	(my-plantuml--exec (read-file-name "puml file: ")))
    (my-plantuml--exec (read-file-name "puml file: "))))

;; private
(defun my-plantuml--exec (plantuml-file)
  "plantuml-flie: e.g c:/Temp/PlantUML/uml.puml"
  (let* ((java      (my-plantuml--path-drive-win2msys2 my-plantuml-java))
	 (plantuml  (my-plantuml--path-drive-win2msys2 my-plantuml-plantuml))
	 (puml-file (my-plantuml--path-drive-win2msys2 (expand-file-name plantuml-file))) ; to be safety, call expand-file-name
	 ;;(cmd    (format "%s -jar %s -charset UTF-8 %s" java plantuml file))
	 (cmd (mapconcat #'shell-quote-argument
			 (list java "-jar" plantuml "-charset" "UTF-8" puml-file) " "))
	 ret)
    ;; *scratch* pass
    ;; "/C/Program\\ Files\\ \\(x86\\)/Common\\ Files/Oracle/Java/javapath/java.exe -jar /C/Program\\ Files/PlantUML/plantuml.1.2020.15.jar /C/Temp/Plantuml/uml.puml
    ;; on fish shell pass
    ;; /C/Program\ Files\ \(x86\)/Common\ Files/Oracle/Java/javapath/java.exe -jar /C/Program\ Files/PlantUML/plantuml.1.2020.15.jar uml.txt
    ;; executed on fish shell
    (message "[my-plantuml] --exec: %s" cmd)
    (setq ret (shell-command-to-string cmd))
    (if (string-equal ret "") ; success
	(progn
	 (message "[my-plantuml] --exec: compiled")
	 ;(run-associated-program (format "%s.png" (file-name-sans-extension file))))
	 (w32-shell-execute "open" (format "%s.png" (file-name-sans-extension plantuml-file))))
      (message "[my-plantuml] --exec error: %s" ret))))

(defun my-plantuml--path-drive-win2msys2 (path)
  "convert msys2path to elisp interanal"
  ;; drive C: -> /C/
  (replace-regexp-in-string "^\\([a-zA-Z]\\):" "/\\1" path)
  ;; path, escape ' ', '(' , ')' with \\
  ;;(replace-regexp-in-string "\\( \\|(\\|)\\)" "\\\\\\1" epath) ; no use, alternate mapconcat #'shell-qutoe-argument in my-plantuml--exec
  )

(provide 'my-plantuml)
;;; my-plantuml.el ends here
