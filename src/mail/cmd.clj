(ns mail.cmd
  "Functions for commandline scripts."
  (:require [mail.core :as core])
  (:require [mail.util :as util])
  (:require mail.source.javax) 
  (:require mail.sink.sqlite)) 

(defn split-str-at [s ch]
  (into [] (.split s ch))) 

(defn init 
  "Call the init function named by spec with args."
  [spec & args]
  (let [init-fn (eval (symbol (str "init-" spec)))]
    (init-fn args))) 

(defn init-javax [[props]]
  (mail.source.javax/make-session (util/props-from-file props))) 

(defn init-sqlite [[filename]]
  (mail.sink.sqlite/create-with-file filename))

(defn- int-arg [args i default]
  (if-let [arg (nth args i)]
    (try
      (Integer/parseInt arg)
      (catch IndexOutOfBoundsException e default))
    default))

(defn copy [[sourcespec sinkspec & args]]
  (let [source (apply init (split-str-at sourcespec ":"))
        sink (apply init (split-str-at sinkspec ":"))
        start (int-arg args 0 1)
        end (int-arg args 1 (count source))]
    (core/copy-range source sink (range start end)))) 

(defn pcopy [[threads sourcespec sinkspec & args]]
  (let [source #(apply init (split-str-at sourcespec ":"))
        sink #(apply init (split-str-at sinkspec ":"))
        start (int-arg args 0 1)
        end (int-arg args 1 (count (source)))]
    (core/parallel-copy source sink (range start end) (Integer/parseInt threads))
    (shutdown-agents))) 

(defn main [[cmd & args]]
  ((eval (symbol cmd)) args))

(if *command-line-args* (main *command-line-args*))
