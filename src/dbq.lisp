(in-package :dbq)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defun class-to-table-name (class)
  (concatenate 'string "`"
               (substitute #\_ #\- (pluralize (string-downcase (class-name class))))
               "`"))

(defun slot-to-column-name (slot)
  (concatenate 'string
               "`"
               (substitute #\_ #\- (string-downcase slot))
               "`"))

(defmethod save (record)
  (if (slot-boundp record 'id)
      (update record)
      (create record)))

(defun insert-columns (record)
  (loop for slot in (sb-pcl:class-slots (class-of record))
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (slot-boundp record slot-name)
          collect (slot-to-column-name slot-name)))

(defun insert-values (record)
  (loop for slot in (sb-pcl:class-slots (class-of record))
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name) (string-not-equal "id" slot-name))
          collect (to-sql-value (slot-value record slot-name))))

(defmethod create (record)
  (destructuring-bind (_a (((id)) _b))
      (execute (format nil "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~}); select last_insert_id()"
                       (class-to-table-name (class-of record))
                       (insert-columns record)
                       (insert-values record)))
    (declare (ignore _a _b))
    (setf (slot-value record 'id) id)))


;; (defmethod create (record)
;;   (multiple-value-bind (columns values) (%columns-and-value-of record :except id)
;;     (execute
;;      (format nil "insert into ~a (~{~a~^,~}) values (~{~a^,~})"
;;              (table-name-of record)
;;              columns
;;              values))))

;; (defmethod %columns-and-value-of (record &key expect)
;;   (let ((expect (ensure-list expect)))
;;     (loop for slot in (c2mop:class-slots (class-of record))
;;           if (typep slot 'column-effective-slot-definition)
;;             collect slot)))
