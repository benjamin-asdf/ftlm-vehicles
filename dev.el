(defun ftlm-hearts-shadow-cljs-connect-app-client ()
  (interactive)
  (let ((cider-shadow-default-options ":client"))
    (cider-connect-cljs
     '(:project-dir
       "/home/benj/repos/clojure/ftlm-hearts" 
       :cljs-repl-type shadow
       :host "localhost"
       :port
       7013))))
