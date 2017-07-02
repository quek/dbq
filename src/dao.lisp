(in-package :dbq)

(defmethod save (record)
  (if (slot-boundp record 'id)
      (update record)
      (create record)))

(defun column-slots (record)
  (loop with has-many = (ignore-errors (slot-value record 'has-many))
        for slot in (sb-pcl:class-slots (class-of record))
        if (and (eq (sb-pcl:slot-definition-allocation slot) :instance)
                (not (member (sb-pcl:slot-definition-name slot) has-many)))
          collect slot))

(defun insert-columns (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name) (string-not-equal "id" slot-name))
          collect (to-column-name slot-name)))

(defun insert-values (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name) (string-not-equal "id" slot-name))
          collect (to-sql-value (slot-value record slot-name))))

(defun update-set (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name)
                (string-not-equal "id" slot-name)
                (string-not-equal "created-at" slot-name))
          append (list (to-column-name slot-name)
                       (to-sql-value
                        (if (string-equal "updated-at" slot-name)
                            (local-time:now)
                            (slot-value record slot-name))))))

(defmethod create (record)
  (destructuring-bind (_a (((id)) _b))
      (execute (format nil "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~}); select last_insert_id()"
                       (to-table-name record)
                       (insert-columns record)
                       (insert-values record)))
    (declare (ignore _a _b))
    (setf (slot-value record 'id) id)))

(defmethod update (record)
  (execute (format nil "update ~a set ~{~a=~a~^, ~} where id=~d"
                   (to-table-name record)
                   (update-set record)
                   (slot-value record 'id))))

(defmethod delete-from (record)
  (execute (format nil "delete from ~a where id=~d"
                   (to-table-name record)
                   (slot-value record 'id))))
