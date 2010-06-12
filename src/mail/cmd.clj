(ns mail.cmd
  (:require [mail.core :as core])
  (:require [mail.util :as util])
  (:require mail.source.javax) 
  (:require mail.sink.sqlite)) 

(defn split-str-at [s ch]
  (into [] (.split s ch))) 

(defn init [spec & args]
  (let [init-fn (eval (symbol (str "init-" spec)))]
    (init-fn args))) 

(defn init-javax [[props]]
  (mail.source.javax/make-session (util/props-from-file props))) 

(defn init-sqlite [[filename]]
  (mail.sink.sqlite/create-with-file filename))

(defn copy [[sourcespec sinkspec & args]]
  (let [source (apply init (split-str-at sourcespec ":"))
        sink (apply init (split-str-at sinkspec ":"))
        inbox (.default-folder source)
        start (nth args 0 1)
        end (nth args 1 1000)]
    (core/copy-range inbox sink (range start end)))) 

(defn main [[cmd & args]]
  ((eval (symbol cmd)) args))

(if *command-line-args* (main *command-line-args*))
