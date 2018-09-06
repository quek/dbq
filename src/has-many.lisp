(in-package :dbq)

(defun update-has-many (record)
  "新しいリストにないものは削除してみよう。
問題があるようなら _delete スロット追加とか検討する。"
  (loop with id = (id-of record)
        with class = (class-name (class-of record))
        for slot in (has-many-slots class)
        for reldat = (reldat class slot)
        for other-class = (slot-value reldat 'other-class)
        for foreign-key-slot = (slot-value reldat 'foreign-key)
        for through = (slot-value reldat 'through)
        if (and (null through) (slot-boundp record slot))
          do (let ((old-list (fetch (query other-class (where foreign-key-slot id)))))
              (loop for x in (slot-value record slot)
                    do (setf (slot-value x (slot-value reldat 'foreign-key))
                             id)
                       (save x)
                       (setf old-list (cl:delete x old-list :test #'id=)))
               (loop for old in old-list
                     do (delete old)))))

(defun preload-has-many (records query)
  (loop with class = (query-builder-from query)
        with ids = (delete-duplicates (mapcar #'id-of records))
        for slot in (intersection (query-builder-preload query)
                                  (has-many-slots class))
        for reldat = (reldat class slot)
        for other-class = (slot-value reldat 'other-class)
        for foreign-key-slot = (slot-value reldat 'foreign-key)
        for children = (fetch (query other-class (where foreign-key-slot ids)))
        do (loop for record in records
                 do (setf (slot-value record slot) nil))
           (loop for child in (nreverse children)
                 for record = (find (slot-value child foreign-key-slot)
                                    records :key #'id-of)
                 do (push child (slot-value record slot)))))

