(in-package :dbq)

(defclass id-mixin ()
  ((id :initarg :id :accessor id-of)))

(defmethod id= ((a id-mixin) (b id-mixin))
  (= (id-of a) (id-of b)))

(defclass created-at-mixin ()
  ((created-at :initarg :created-at :accessor created-at)))

(defclass updated-at-mixin ()
  ((updated-at :initarg :updated-at :accessor updated-at)))

(defclass dao-mixin (id-mixin created-at-mixin updated-at-mixin)
  ())

(defun persistedp (record)
  (slot-boundp record 'id))

(defmethod save (record)
  (prog1 (if (slot-boundp record 'id)
             (update record)
             (create record))
    (update-hbtm record)
    (update-has-many record)))

(defun column-slots (record)
  (loop for slot in (sb-pcl:class-slots (class-of record))
        if (and (eq (sb-pcl:slot-definition-allocation slot) :instance)
                (char/= #\% (char (symbol-name (sb-pcl:slot-definition-name slot)) 0)))
          collect slot))

(defun normal-column-p (record slot-name)
  (and (not (hbtm-slot-p record slot-name))
       (not (has-many-slot-p record slot-name))
       (not (belongs-to-slot-p record slot-name))))

(defun insert-columns-values (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name)
                (string-not-equal "id" slot-name)
                (normal-column-p record slot-name))
          collect slot-name into columns
          and
            collect (slot-value record slot-name) into values
        finally (return (values columns values))))


(defun update-set (record)
  (loop for slot in (column-slots record)
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (and (slot-boundp record slot-name)
                (not (eq 'id slot-name))
                (not (eq 'created-at slot-name))
                (normal-column-p record slot-name))
          append (list slot-name
                       (slot-value record slot-name))))

(defun insert-sql (record)
  (multiple-value-bind (columns values) (insert-columns-values record)
    (format nil "insert into ~/dbq::tbl/ (~{~/dbq::col/~^, ~}) values (~{~/dbq::val/~^, ~})"
            record
            columns
            values)))

(defmethod create (record)
  (destructuring-bind (((_ . id)))
      (execute (concatenate 'string (insert-sql record) " returning id"))
    (declare (ignore _))
    (setf (slot-value record 'id) id))
  record)

(defmethod create :before ((record created-at-mixin))
  (unless (slot-boundp record 'created-at)
    (setf (slot-value record 'created-at) (local-time:now))))

(defmethod create :before ((record updated-at-mixin))
  (unless (slot-boundp record 'updated-at)
    (setf (slot-value record 'updated-at) (local-time:now))))

(defun update-sql (record)
  (format nil "~
update ~/dbq::tbl/ set ~{~/dbq::col/=~/dbq::val/~^, ~} where id=~/dbq::val/"
          record
          (update-set record)
          (slot-value record 'id)))

(defmethod update (record)
  (execute (update-sql record)))

(defmethod update :before ((record updated-at-mixin))
  (setf (slot-value record 'updated-at) (local-time:now)))

(defmethod delete (record)
  (execute (format nil "delete from ~/dbq::tbl/ where id=~/dbq::val/"
                   record
                   (slot-value record 'id))))
