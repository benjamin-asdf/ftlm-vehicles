(defun ftlm-vehicles-cider-jack-in ()
  (interactive)
  (let ((cider-clojure-cli-aliases '(":dev:cljs:"))
        (cider-preferred-build-tool 'clojure-cli)))
  (cider-jack-in-clj
   '(:project-dir "/home/benj/repos/clojure/vehicles/")))

(defun ftlm-vehicles-cider-shadow-cljs-connect-app-client ()
  (interactive)
  (let ((cider-shadow-default-options ":client"))
    (cider-connect-cljs
     '(:project-dir "/home/benj/repos/clojure/vehicles/"
                    :cljs-repl-type shadow
                    :host "localhost"
                    :port 7014))))

(ftlm-vehicles-cider-jack-in)
