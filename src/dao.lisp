(in-package :dbq)

(defclass id-mixin ()
  ((id :initarg :id :accessor id-of)))

(defclass created-at-mixin ()
  ((created-at :initarg :created-at :accessor created-at)))

(defclass updated-at-mixin ()
  ((updated-at :initarg :updated-at :accessor updated-at)))

(defclass dao-mixin (id-mixin created-at-mixin updated-at-mixin)
  ())

(defun persistedp (record)
  (slot-boundp record 'id))

(defmethod save (record)
  (if (slot-boundp record 'id)
      (update record)
      (create record)))

(defun column-slots (record)
  (loop for slot in (sb-pcl:class-slots (class-of record))
        if (eq (sb-pcl:slot-definition-allocation slot) :instance)
          collect slot))

(defun normal-column-p (record slot-name)
  (and (not (hbtm-slot-p record slot-name))
       (not (has-many-slot-p record slot-name))))

(defun insert-columns-values (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name)
                (string-not-equal "id" slot-name)
                (normal-column-p record slot-name))
          collect (to-column-name slot-name) into columns
          and
            collect (to-sql-value (slot-value record slot-name)) into values
        finally (return (values columns values))))


(defun update-set (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name)
                (not (eq 'id slot-name))
                (not (eq 'created-at slot-name)))
          append (list (to-column-name slot-name)
                       (to-sql-value (slot-value record slot-name)))))

(defun insert-sql (record)
  (multiple-value-bind (columns values) (insert-columns-values record)
    (format nil "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
            (to-table-name record)
            columns
            values)))

(defmethod create (record)
  (destructuring-bind (_a (((id)) _b))
      (execute (concatenate 'string (insert-sql record) "; select last_insert_id();"))
    (declare (ignore _a _b))
    (setf (slot-value record 'id) id))
  (insert-hbtm record)
  record)

(defmethod create :before ((record created-at-mixin))
  (unless (slot-boundp record 'created-at)
    (setf (slot-value record 'created-at) (local-time:now))))

(defmethod create :before ((record updated-at-mixin))
  (unless (slot-boundp record 'updated-at)
    (setf (slot-value record 'updated-at) (local-time:now))))

(defmethod update (record)
  (execute (format nil "update ~a set ~{~a=~a~^, ~} where id=~d"
                   (to-table-name record)
                   (update-set record)
                   (slot-value record 'id))))

(defmethod update :before ((record updated-at-mixin))
  (setf (slot-value record 'updated-at) (local-time:now)))

(defmethod delete-from (record)
  (execute (format nil "delete from ~a where id=~d"
                   (to-table-name record)
                   (slot-value record 'id))))
