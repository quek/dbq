(in-package :dbq)

(defgeneric belongs-to-slot-p (x slot)
  (:method ((class-name symbol) slot)
    (aand  (gethash class-name *belongs-to*)
           (gethash slot it)))
  (:method (record slot)
    (belongs-to-slot-p (class-name (class-of record)) slot)))

(defun preload-belongs-to (records query)
  (when records
    (loop with class = (class-name (class-of (car records))) ;STI で動かないよね・・・
          for slot in (intersection (query-builder-preload query)
                                    (belongs-to-slots class))
          for reldat = (reldat class slot)
          for other-class = (slot-value reldat 'other-class)
          for foreign-key-slot = (slot-value reldat 'foreign-key)
          for ids = (delete-duplicates (mapcar (lambda (b) (slot-value b foreign-key-slot)) records)
                                       :test #'equal)
          for parents = (fetch (query other-class (where :id ids)))
          do (loop for record in records
                   for parent = (find (slot-value record foreign-key-slot) parents :key #'id-of
                                                                                   :test #'equal)
                   do (setf (slot-value record slot) parent)))))
