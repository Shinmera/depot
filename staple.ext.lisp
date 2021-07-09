(asdf:load-systems :staple-markless :depot-zip :depot-in-memory)

(defpackage "depot-docs"
  (:use #:cl)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)))

(defmethod staple:document-package ((page staple:simple-page)) (find-package "depot-docs"))

(defmethod staple:subsystems ((system (eql (asdf:find-system :depot))))
  (list (asdf:find-system :depot-zip)
        (asdf:find-system :depot-in-memory)))
