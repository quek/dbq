(asdf:defsystem :dbq
  :version "0.0.1"
  :license "BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "format")
               (:file "connection")
               (:file "query")
               (:file "dao")
               (:file "hbtm")
               (:file "has-many")
               (:file "belongs-to")
               (:file "migration")
               (:file "json"))
  :depends-on (#:alexandria
               #:postmodern
               #:closer-mop
               #:anaphora
               #:cl-ppcre
               #:local-time
               #:cl-json
               #:log4cl))

