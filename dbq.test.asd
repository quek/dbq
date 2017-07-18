(asdf:defsystem :dbq.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "sql")
               (:file "class")
               (:file "relation")
               (:file "test"))
  :depends-on (:dbq
               :fiasco))

