(defpackage #:sh-utils
  (:use #:cl)
  (:import-from #:inferior-shell
                #:pipe
                #:run/nil)
  (:import-from #:jonathan
                #:parse)
  (:import-from #:serapeum
                #:defalias)
  (:import-from #:str
                #:concat
                #:ends-with?)
  (:import-from #:uiop
                #:chdir
                #:quit
                #:getcwd)
  (:export #:defpackage-sh
           #:parse-json
           #:mkdirp
           #:takedir
           #:echo
           #:pwd
           #:notify
           #:notify-success
           #:notify-failure
           #:sh-if))

(in-package #:sh-utils)

(defalias parse-json #'parse)

(defmacro defpackage-sh (package &body options)
  "Wrapper for `defpackage' to automatically import useful helpers. "
  `(progn (defpackage ,package
            (:use #:cl #:sh-utils)
            (:import-from #:anaphora
                          #:it
                          #:acond
                          #:aif)
            (:import-from #:inferior-shell
                          #:pipe
                          #:run/s
                          #:run/nil)
            (:import-from #:str
                          #:concat
                          #:from-file
                          #:to-file)
            (:import-from #:uiop
                          #:chdir
                          #:quit
                          #:getcwd)
            ,@options)
          (in-package ,package)))
