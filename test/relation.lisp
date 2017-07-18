(in-package :dbq.test)

(def-has-many :class entry :slot comments :other-class comment)
(def-hbtm :class entry :slot categories :other-class category :table "category_entries")
(def-belongs-to :class entry :slot user)
