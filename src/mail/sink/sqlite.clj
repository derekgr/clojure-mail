(ns mail.sink.sqlite
  "Writes core.Messages to a sqlite database backed by a file."
  (:require [mail.core :as core])
  (:import [java.sql Connection DriverManager Statement PreparedStatement]))

(def *insert-sql* (str 
  "insert or replace into mail (id, \"from\",\"to\",subj,body,tags) values (?,?,?,?,?,?)"))

(defn str-or-null 
  "Apply the str value to position i of the prepared statement, or set null."
  [stmt i vls]
  (let [v (nth vls i)]
    (if v (.setString stmt (inc i) (str v))
      (.setNull stmt (inc i) java.sql.Types/VARCHAR)))) 

(defn flush-stmt 
  "Flush the prepared statement's batch."
  [conn stmt]
  (.setAutoCommit conn false)
  (.executeBatch stmt)
  (.setAutoCommit conn true)
  (.clearBatch stmt))

(defn- apply-to-statement 
  "Apply the bindings to the statment and execute it in batch mode. Increment ref."
  [counter stmt bindings]
  (dorun (map #(str-or-null stmt % bindings) (range 0 (count bindings))))
  (.addBatch stmt)
  (let [ct (swap! counter inc)
        conn (.getConnection stmt)]
    (if (zero? (mod ct 1000))
      (.println System/out (str ct " messages."))
      (flush-stmt conn stmt))))

(defn- create-tables [conn]
  (let [create-stmt (.createStatement conn)]
    (.executeUpdate 
      create-stmt 
      (str
        "create table if not exists mail 
        (id text primary key, \"from\" text, \"to\" text, subj text, body text, tags text)")))) 

(defn create-with 
  "Reify mail.core.Sink with a sqlite database writer with the given connection."
  [conn]
  (let [_ (create-tables conn)
        insert-message (.prepareStatement conn *insert-sql*)
        counter (atom 1)]
      (reify mail.core.Sink
        (write [x msg]
          (let [cols [:id :from :to :subject :body :tags]
                vls (map #(get msg %) cols)]
          (apply-to-statement counter insert-message vls)))
        (flush-messages [x] (flush-stmt conn insert-message)))))

(defn create-with-file 
  "Reify mail.core.Sink with a sqlite database writer to the given file."
  [fname] 
  (let [klass (Class/forName "org.sqlite.JDBC")
        conn (DriverManager/getConnection (str "jdbc:sqlite:" fname))]
    (create-with conn)))
