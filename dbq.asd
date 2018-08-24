(asdf:defsystem :dbq
  :version "0.0.1"
  :license "BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "format")
               (:file "connection")
               (:file "operator")
               (:file "query")
               (:file "dao")
               (:file "hbtm")
               (:file "has-many")
               (:file "belongs-to")
               (:file "has-one")
               (:file "migration"))
  :depends-on (#:alexandria
               #:postmodern
               #:closer-mop
               #:anaphora
               #:cl-ppcre
               #:local-time
               #:fast-io
               #:ieee-floats
               #:log4cl))

