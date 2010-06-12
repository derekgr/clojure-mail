(ns mail.types)

;; A generic record type.
(defrecord Message 
  [id, subject, from, to, body, content-type]) 

(defprotocol Mailboxes
  "Functions related to a collection of mailboxes."
  (default-folder [x] "Returns the default Folder.")
  (folder [x nm] "Returns the Folder with the given name."))

(defprotocol Folder
  "A folder is a logical collection of email messages."
  (message-at [x i] "Returns the Message at position i."))

(defprotocol Sink
  "A Sink can store messages with from, to, subject, and body components."
  (write [x message])
  (flush-messages [x]))

(defn copy-messages 
  "Send the sequence of messages to the sink."
  [s sink]
  (dorun (map #(.write sink %) s))
  (.flush-messages sink))

(defn copy-all-messages [folder sink]
  (copy-messages (seq folder) sink))

(defn source-range [source s] 
  (map #(nth source %) s)) 

(defn copy-range [folder sink s]
  (copy-messages (source-range folder s) sink))

