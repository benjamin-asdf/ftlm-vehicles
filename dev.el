(defun ftlm-vehicles-cider-jack-in ()
  (interactive)
  (let ((cider-clojure-cli-aliases '(":dev:cljs:"))
        (cider-preferred-build-tool 'clojure-cli)))
  (cider-jack-in-clj
   `(:project-dir ,(directory-file-name
                    (expand-file-name "./")))))

(defun ftlm-vehicles-cider-shadow-cljs-connect-app-client ()
  (interactive)
  (let ((cider-shadow-default-options ":client"))
    (cider-connect-cljs
     `(:project-dir ,(directory-file-name
                      (expand-file-name "./"))
                    :cljs-repl-type shadow
                    :host "localhost"
                    :port 7014))))

(ftlm-vehicles-cider-jack-in)
(ftlm-vehicles-cider-shadow-cljs-connect-app-client)
