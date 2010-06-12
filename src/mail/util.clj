(ns mail.util) 

(defn props-from-file 
  "Create instance of java.util.Properties from given path."
  [fname]
  (with-open [reader (java.io.FileReader. fname)]
    (doto (java.util.Properties.) (.load reader)))) 
