(asdf:defsystem :dbq.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "migrations")
               (:file "class")
               (:file "relation")
               (:file "test"))
  :depends-on (:dbq
               :fiasco))

