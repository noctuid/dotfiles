(asdf:defsystem "sh-utils"
  :description "Helper functions for shell scripting"
  :license "GPL3"
  :version "0.1"
  :depends-on ("alexandria"
               "anaphora"
               "cl-ppcre"
               "inferior-shell"
               "jonathan"
               "serapeum"
               "str"
               "trivia"
               "unix-opts")
  :serial t
  :components ((:file "package")
               (:file "util")))
