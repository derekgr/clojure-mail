(ns mailbot)

(require '[mailbot.mail :as mail])
(require '[mailbot.mail.javax :as javax])
(require '[mailbot.store.sqlite :as db])

(defn usage [] 
  (let [usage (str
    "USAGE: mailbot properties-file sqlite-target")]
    (do
      (println usage) 
      (System/exit 1))))

(defn props-from-file 
  "Create instance of java.util.Properties from given path."
  [fname]
  (let [reader (java.io.FileReader. fname)
        config (doto (java.util.Properties.) (.load reader))]
    config))

(defn inbox [props]
  (let [session (javax/make-session props #(.get props "mail.password"))]
    (.default-folder session)))

(defn main 
  ([argv]
   (let [argc (count argv)]
     (when (not (= argc 2)) (usage))
     (let [[propfile sqlite-out] argv
            folder (inbox (props-from-file propfile))
            sink (db/create-with-file sqlite-out)]
            (mail/copy-all-messages folder sink)))))

(if *command-line-args*
  (main *command-line-args*))
