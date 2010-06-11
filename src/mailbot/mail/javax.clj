(ns mailbot.mail.javax
  (:require [mailbot.mail :as mail])
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
      
(defn- decode-multipart 
  "Decode a content part, possibly multipart."
  [body content-type]
  (cond
    (instance? javax.mail.Multipart body)
    (let [bodies 
          (for [i (range 0 (.getCount body)) :let [part (.getBodyPart body i)]] 
            (decode-multipart (.getContent part) (.getContentType part)))]
      (reduce #(merge-with concat %1 %2) {} (remove nil? bodies)))
    (mimetype? content-type "text/plain")
    {:plain (clean body)}
    (mimetype? content-type "text/html")
    {:html (clean-html body)}
    :else
    {:unknown [content-type]}))

(defn choose-body [opts]
  (let [best [:plain :html]]
    (some opts best)))

(defn make-message 
  "Turn a Message into a clojure map."
  ([id m props]
   (let [content-type (.getContentType m)]
     {:id id
      :subject (.getSubject m)
      :from (strs (.getFrom m))
      :to (strs (.getRecipients m javax.mail.Message$RecipientType/TO)) 
      :body (choose-body (decode-multipart (.getContent m) content-type))
      :content-type content-type})))

(defn- message-id [folder message]
  (str message))

(defn- make-folder 
  "Abstract a javax.mail.Folder as something that is countable, seqable, and efficiently
  nth-able in clojure."
  [props jfolder]
  (let [message-count (.getMessageCount jfolder)]
    (do
      (.open jfolder javax.mail.Folder/READ_ONLY)
      (reify 
        clojure.lang.Seqable
        (seq [x] (map #(.message-at x %) (range 1 (inc message-count))))
        clojure.lang.Indexed
        (nth [x i] (if (<= i message-count) (.message-at x i)))
        clojure.lang.Counted
        (count [x] message-count)
        mail/Folder
        (message-at [x i] 
          (let [message (.getMessage jfolder i)
                id (message-id jfolder i)]
            (try
              (make-message id message props)
              (catch Exception e { :id id :body nil :error e }))))
        (close [x] (.close jfolder false))
        ))))

(defn make-session 
  "Abstract the connection sequence to an IMAP mailbox."
  ([config password-fn]
   (let [session (Session/getInstance config)
         props (str-keyword-map config)
         store (connect (.getStore session) props password-fn)]
    (reify mail/Mailboxes
           (default-folder 
             [x]
             (if (contains? props :mail.folder) 
               (.folder x (props :mail.folder))
               (make-folder props (.getDefaultFolder store))))
           (folder [x nm] (make-folder props (.getFolder store nm)))))))

