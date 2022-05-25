(ns bb.util
  "General babashka script helper utilities."
  (:require
   [babashka.process :refer [$ sh]]
   [clojure.java.io :as io]
   [clojure.string :refer [split trim]]))

(defn sh-out
  "Return the trimmed output of a command."
  [cmd]
  (-> (sh cmd) :out trim))

(defmacro $-reader
  "Return the output of a command as a stream using io/reader."
  [& args]
  `(-> ($ ~@args) :out io/reader))
