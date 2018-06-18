(in-package :dbq.test)

(dbq:def-has-many :class entry :slot comments :other-class comment)
(dbq:def-hbtm :class entry :slot categories :other-class category :table "category_entries")
(dbq:def-belongs-to :class entry :slot user)
