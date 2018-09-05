(in-package :dbq)

(defun preload-has-one (records query)
  (loop with class = (query-builder-from query)
        with ids = (delete-duplicates (mapcar #'id-of records))
        for slot in (intersection (query-builder-preload query)
                                  (has-one-slots class))
        for other-class = (has-one-other-class class slot)
        for foreign-key-slot = (has-one-foreign-key-slot class slot)
        for children = (fetch (query other-class (where foreign-key-slot ids)))
        do (loop for record in records
                 do (setf (slot-value record slot) nil))
           (loop for child in (nreverse children)
                 for record = (find (slot-value child foreign-key-slot)
                                    records :key #'id-of)
                 do (setf (slot-value record slot) child))))

