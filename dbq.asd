(asdf:defsystem :dbq
  :version "0.0.1"
  :license "BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "format")
               (:file "connection")
               (:file "dao")
               (:file "hbtm")
               (:file "has-many")
               (:file "belongs-to")
               (:file "query"))
  :depends-on (#:alexandria
               #:cl-mysql
               #:closer-mop
               #:anaphora
               #:cl-ppcre
               #:local-time))

