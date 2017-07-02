(in-package :dbq)

(defvar *query-builder* nil)

(defstruct query-builder
  (select "*")
  from
  where
  order
  having
  group
  limit
  offset)

(defgeneric to-query-builder (x)
  (:method ((x query-builder))
    x)
  (:method ((x symbol))
    (make-query-builder :from (to-table-name x))))

(defun select (select)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-select *query-builder*) select)
  *query-builder*)

(defun from (from)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-from *query-builder*) from)
  *query-builder*)

(defun where (&rest where)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-where *query-builder*)
        (append (query-builder-where *query-builder*) where))
  *query-builder*)

(defun limit (limit)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-limit *query-builder*) limit)
  *query-builder*)

(defun sql (query-builder)
  (with-output-to-string (*standard-output*)
    (format t
            "select ~a from ~a"
            (query-builder-select query-builder)
            (query-builder-from query-builder))
    (let ((where (query-builder-where query-builder)))
      (when where
          (write-string " where ")
          (loop with and = ""
                while where
                do (write-string and)
                   (setf and " and ")
                   (let ((x (pop where)))
                     (format t "~a=~a"
                             (to-column-name x)
                             (to-sql-value (pop where)))))))
    (when (query-builder-limit query-builder)
      (format t " limit ~d" (to-sql-value (query-builder-limit query-builder))))))

(defmacro query (query-builder &body body)
  `(let ((*query-builder* (to-query-builder ,query-builder)))
     ,@body))

(defun set-value (object slot-name value)
  (let ((slot (find slot-name (sb-mop:class-slots (class-of object))
                    :test #'string-equal
                    :key #'sb-mop:slot-definition-name)))
    (if slot
        (setf (slot-value object (sb-mop:slot-definition-name slot)) value)
        (format t "no slot for ~a ~a" slot-name value))))

(defun store (class rows columns)
  (loop for row in rows
        collect (let ((object (make-instance class)))
                  (loop for value in row
                        for (column-name column-type) in columns
                        do (set-value object column-name value))
                  object)))

(defun fetch-one (query &key class)
  (destructuring-bind ((rows columns)) (execute (sql (query query (limit 1))))
    (car (store class rows columns))))


