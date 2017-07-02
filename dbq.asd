(asdf:defsystem :dbq
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "function")
               (:file "dao")
               (:file "query"))
  :depends-on (#:alexandria
               #:cl-mysql
               #:closer-mop
               #:anaphora
               #:cl-ppcre
               #:local-time
               #:info.read-eval-print.double-quote))

