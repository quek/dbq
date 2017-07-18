(in-package :dbq)

(defvar *query-builder* nil)

(defstruct query-builder
  select
  from
  join
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
    (make-query-builder :from x)))

(defun select (select)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-select *query-builder*) select)
  *query-builder*)

(defun from (from)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-from *query-builder*) from)
  *query-builder*)

(defun join (&rest join)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-join *query-builder*)
        (append (query-builder-join *query-builder*) join))
  *query-builder*)

(defun where (&rest where)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (push where (query-builder-where *query-builder*))
  *query-builder*)

(defun order (order)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-order *query-builder*) order)
  *query-builder*)

(defun limit (limit)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-limit *query-builder*) limit)
  *query-builder*)

(defun sql (query-builder)
  (let ((class-symbol (query-builder-from query-builder)))
    (with-output-to-string (*standard-output*)
      (write-string "select ")
      (aif (query-builder-select query-builder)
           (format t "~{~/dbq::col/~^ ,~}" (alexandria:ensure-list it))
           (format t "distinct ~/dbq::tbl/.*" class-symbol))
      (format t " from ~/dbq::tbl/" class-symbol)
      (loop for join in (query-builder-join query-builder)
            for join-clause = (or (hbtm-join-clause class-symbol join)
                                  (has-many-join-clause class-symbol join)
                                  (belongs-to-join-clause class-symbol join))
            if join-clause
              do (write-char #\space)
                 (write-string join-clause))
      (sql-where query-builder)
      (awhen (query-builder-order query-builder)
        (write-string " order by ")
        (write-string it))
      (when (query-builder-limit query-builder)
        (format t " limit ~/dbq::val/" (query-builder-limit query-builder))))))

(defun sql-where (query-builder)
  (let ((wheres (query-builder-where query-builder)))
    (when wheres
      (format t " where ~{~a~^ and ~}"
              (loop for where in wheres
                    if (null (cdr where))
                      append where
                    else
                      append (loop for (col val) on where by #'cddr
                                   collect (format nil "~/dbq::col/=~/dbq::val/" col val)))))))

(defmacro query (query-builder &body body)
  `(let ((*query-builder* (to-query-builder ,query-builder)))
     ,@body
     *query-builder*))

(defun set-value (object value column-name column-type)
  (let ((slot (find (to-column-name column-name) (sb-mop:class-slots (class-of object))
                    :test #'string-equal
                    :key (lambda (x) (to-column-name (sb-mop:slot-definition-name x))))))
    (if slot
        (setf (slot-value object (sb-mop:slot-definition-name slot))
              (to-lisp-value value column-type))
        (format t "no slot for ~a ~a" column-name value))))

(defun store (class rows columns)
  (loop for row in rows
        collect (let ((object (make-instance class)))
                  (loop for value in row
                        for (column-name column-type) in columns
                        do (set-value object value column-name column-type))
                  object)))

(defun fetch-one (query &key (class (query-builder-from query)))
  (destructuring-bind ((rows columns)) (execute (sql (query query (limit 1))))
    (car (store class rows columns))))

(defun fetch (query &key (class (query-builder-from query)))
  (destructuring-bind ((rows columns)) (execute (sql query))
    (store class rows columns)))

(defun find-by (class &rest conditions)
  (fetch-one (query class (apply #'where conditions)) :class class))
