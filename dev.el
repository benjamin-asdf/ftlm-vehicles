(defun ftlm-vehicles-shadow-cljs-connect-app-client ()
  (interactive)
  (let ((cider-shadow-default-options ":client"))
    (cider-connect-cljs
     '(:project-dir
       "/home/benj/repos/clojure/ftlm-vehicles" 
       :cljs-repl-type shadow
       :host "localhost"
       :port 7014))))
