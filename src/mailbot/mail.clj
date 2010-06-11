(ns mailbot.mail)

(defprotocol Mailboxes
  "Functions related to a collection of mailboxes."
  (default-folder [x] "Returns the default Folder.")
  (folder [x nm] "Returns the Folder with the given name."))

(defprotocol Folder
  "A folder is a logical collection of email messages."
  (message-at [x i] "Returns the Message at position i.")
  (close [x] "Disposes of any resources held opening the folder."))

(defprotocol Sink
  "A Sink can store messages with from, to, subject, and body components."
  (write [x message])
  (flush-messages [x]))

(defn debug [msg]
  (.println System/err msg))

(defn debug-body [m]
  (if (:body m) true
      (if (:error m) (debug (str "Message " (:id m) ": " (.getMessage (:error m)))))))

(defn copy-messages 
  "Send the sequence of messages to the sink."
  [s sink]
  (dorun (map #(.write sink %) (filter debug-body s)))
  (.flush-messages sink))

(defn copy-all-messages [folder sink]
  (copy-messages (seq folder) sink))

(defn copy-range [folder sink a b]
  (copy-messages (map #(nth folder %) (range a b)) sink))
