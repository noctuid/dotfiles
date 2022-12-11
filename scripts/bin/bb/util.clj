(ns bb.util
  "General babashka script helper utilities."
  (:require
   [babashka.process :refer [sh process destroy-tree]]
   [clojure.java.io :as io]
   [clojure.string :refer [split trim]]))

(defn sh-out
  "Return the trimmed output of a command."
  [cmd]
  (-> (sh cmd) :out trim))

(defn cmd-reader
  "Return the output of a command as a stream using io/reader."
  [cmd]
  (io/reader (:out (process cmd {:shutdown destroy-tree}))))

(defn env
  "Return the value of an environment variable."
  [environment-variable]
  (System/getenv environment-variable))
