(ns mail.source.javax
  (:require [mail.core :as core])
  (:require [clojure.contrib.logging :as log])
  (:import (javax.mail Session)))

(defn- connect 
 "Given a javax.mail.Store, config, and function yielding a password, change state 
 in the store object to initiate a connection." 
  [store props pwfn]
  (let [host (props :mail.host) 
        user (props :mail.user) 
        password (pwfn)] 
    (doto store (.connect host user password)))) 

(defn- str-keyword-map 
 "Take a java.util.Hashmap and return a clojure map of keyword -> String pairs." 
  [coll]
  (into {} (map (fn [e] [(keyword (.getKey e)) (.getValue e)]) coll)))

(defn- join ([] nil) ([x] x) ([x y] (str x "," y)))

(defn- strs [arr]
  (reduce join (map str (into [] arr))))

(defn- mimetype? [header mime]
  (.startsWith (.toLowerCase header) mime))

(defn clean [text]
  (.replaceAll text "[\\s\\xA0]+" " "))

(defn strip-html-tags [text]
  (.replaceAll text "<.*?>" ""))

(defn unentize [text]
  (org.apache.commons.lang.StringEscapeUtils/unescapeHtml text))

(defn clean-html [text]
  (clean (strip-html-tags (unentize text))))

(defn- multipart-seq [^javax.mail.Multipart body]
  (for [i (range 0 (.getCount body))] (.getBodyPart body i))) 

(defn- mimetype-concat [k]
  (fn [a b]
    (if (and (sequential? a) (sequential? b))
      (concat a b)
      (throw 
        (IllegalArgumentException. (str "Multiple body values for content-type: " k))))))

(defn- decode-multipart 
  "Decode a content part, possibly multipart."
  [body content-type]
  (cond
    (instance? javax.mail.Multipart body)
    (let [bodies 
          (map #(decode-multipart (.getContent %) (.getContentType %)) (multipart-seq body))] 
      (reduce #(merge-with (mimetype-concat content-type) %1 %2) {} (remove nil? bodies)))
    (mimetype? content-type "text/plain")
    {:plain (clean body)}
    (mimetype? content-type "text/html")
    {:html (clean-html body)}
    :else
    {:unknown [content-type]}))

(defn choose-body [opts]
  (let [best [:plain :html]]
    (some opts best)))

(defn body-or-nil [message]
  (try 
    (choose-body (decode-multipart (.getContent message) (.getContentType message)))
  (catch Exception e 
    (do (log/debug "Exception decoding message body." e) nil))))

(defn make-message 
  "Turn a Message into a clojure map."
  ([id m props]
   (let [content-type (.getContentType m)
         subject (.getSubject m)
         from (strs (.getFrom m))
         to (strs (.getRecipients m javax.mail.Message$RecipientType/TO)) 
         body (body-or-nil m)]
     (mail.core.Message. id subject from to body content-type))))
      
(defn- message-id [folder message]
  (str message))

(defn- message-with-reopen 
  ([jfolder i] (message-with-reopen jfolder i 1))
  ([jfolder i tries]
   (try (.getMessage jfolder i)
     (catch IllegalStateException e 
       (if (> tries 0) 
         (do (.open jfolder javax.mail.Folder/READ_ONLY)
           (message-with-reopen jfolder i (dec tries)))
         (throw e))))))

(defn- make-folder 
  "Abstract a javax.mail.Folder as something that is countable, seqable, and efficiently
  nth-able in clojure."
  [props jfolder]
  (let [message-count (.getMessageCount (doto jfolder (.open javax.mail.Folder/READ_ONLY)))]
    (reify 
      clojure.lang.Seqable
      (seq [x] (map #(.message-at x %) (range 1 (inc message-count))))
      clojure.lang.Indexed
      (nth [x i] (if (<= i message-count) (.message-at x i)))
      clojure.lang.Counted
      (count [x] message-count)
      mail.core.Folder
      (message-at [x i] 
         (let [message (message-with-reopen jfolder i)
               id (message-id jfolder i)]
           (make-message id message props))))))

(defn make-session 
  "Abstract the connection sequence to an IMAP mailbox."
  ([config password-fn]
   (let [session (Session/getInstance config)
         props (str-keyword-map config)
         store (connect (.getStore session) props password-fn)]
    (reify mail.core.Mailboxes
           (default-folder 
             [x]
             (if (contains? props :mail.folder) 
               (.folder x (props :mail.folder))
               (make-folder props (.getDefaultFolder store))))
           (folder [x nm] (make-folder props (.getFolder store nm))))))
  ([config] (make-session config #(.getProperty config "mail.password"))))

