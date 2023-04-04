(ns bb.util
  "General babashka script helper utilities."
  (:require
   [babashka.process :refer [sh process destroy-tree]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :refer [trim]]))

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

;; https://github.com/jafingerhut/jafingerhut.github.com/blob/master/clojure-info/using-edn-safely.md#executive-summary
(defn pr-edn
  "Print forms to *out* as edn."
  [& xs]
  (binding [*print-length*   nil
            *print-dup*      nil
            *print-level*    nil
            *print-readably* true]
    (apply pr xs)))

(defn read-edn
  "Read an edn file."
  [path]
  (with-open [r (io/reader path)]
    (edn/read (java.io.PushbackReader. r))))

(defmacro loop-lines
  "Macro meant to loop over the output lines of cmd.
  Bindings are loop bindings to make.  It should be in the
  form [line (read-line) ...].  The first binding should be the line of output.
  Stop if the line ever becomes nil.  The remaining bindings should be any extra
  loop bindings (that don't use line)."
  {:style/indent 2}
  [cmd bindings & body]
  (let [bind-var (bindings 0)]
    `(with-open [rdr# (cmd-reader ~cmd)]
       (binding [~'*in* rdr#]
         (loop [~bind-var ~(bindings 1)
                ~@(drop 2 bindings)]
           (when ~bind-var
             ~@body))))))
