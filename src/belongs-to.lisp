(in-package :dbq)

(defgeneric belongs-to-slot-p (x slot)
  (:method ((class-name symbol) slot)
    (aand  (gethash class-name *belongs-to*)
           (gethash slot it)))
  (:method (record slot)
    (belongs-to-slot-p (class-name (class-of record)) slot)))
