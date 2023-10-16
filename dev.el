(defun ftlm-vehicles-cider-shadow-cljs-connect-app-client ()
  (interactive)
  (let ((cider-shadow-default-options ":client"))
    (cider-connect-cljs
     '(:project-dir
       "/home/benj/repos/clojure/vehicles/" 
       :cljs-repl-type shadow
       :host "localhost"
       :port 7014))))
