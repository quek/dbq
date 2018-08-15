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
  offset
  page
  (per-page 10)
  preload)

(defmethod to-sql-value ((query-builder query-builder))
  (format nil "(~a)" (sql query-builder)))

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

(defun group (group)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-group *query-builder*) group)
  *query-builder*)

(defun order (order)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-order *query-builder*) order)
  *query-builder*)

(defun limit (limit)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-limit *query-builder*) limit)
  *query-builder*)

(defun offset (offset)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-offset *query-builder*) offset)
  *query-builder*)

(defun page (page)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-page *query-builder*) page)
  *query-builder*)

(defun per-page (per-page)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf (query-builder-per-page *query-builder*) per-page)
  *query-builder*)

(defun preload (&rest preload)
  (setf *query-builder* (copy-query-builder *query-builder*))
  (setf  (query-builder-preload *query-builder*)
         (append (query-builder-preload *query-builder*) preload))
  *query-builder*)

(defun sql (query-builder)
  (with-output-to-string (out)
    (build-select query-builder out)
    (build-from query-builder out)
    (build-join query-builder out)
    (build-where query-builder out)
    (build-group query-builder out)
    (awhen (query-builder-order query-builder)
      (write-string " order by " out)
      (write-string it out))

    (multiple-value-bind (limit offset)
        (let ((page (query-builder-page query-builder)))
          (if page
              (let ((per-page (query-builder-per-page query-builder)))
                (values per-page
                        (* per-page (1- page))))
              (values (query-builder-limit query-builder)
                      (query-builder-offset query-builder))))
      (when limit
        (format out " limit ~/dbq::val/" limit))
      (when offset
        (format out " offset ~/dbq::val/" offset)))))

(defun count-sql (query-builder)
  (with-output-to-string (out)
    (write-string "select count(*)" out)
    (build-from query-builder out)
    (build-join query-builder out)
    (build-where query-builder out)
    (build-group query-builder out)))

(defun build-select (query-builder out)
  (write-string "select " out)
  (let ((select (query-builder-select query-builder))
        (class (query-builder-from query-builder)))
    (cond (select
           (if (stringp select)
               (format out "~a" select)
               (format out "~{~/dbq::col/~^ ,~}" (alexandria:ensure-list select))))
          ((query-builder-group query-builder)
           (format out "~/dbq::tbl/.*" class))
          ((query-builder-join query-builder)
           (format out "distinct ~/dbq::tbl/.*" class))
          (t
           (format out "~/dbq::tbl/.*" class)))))

(defun build-from (query-builder out)
  (format out " from ~/dbq::tbl/" (query-builder-from query-builder)))

(defun build-join (query-builder out)
  (let ((class (query-builder-from query-builder)))
   (loop for join in (query-builder-join query-builder)
         for join-clause = (or (hbtm-join-clause class join)
                               (has-many-join-clause class join)
                               (belongs-to-join-clause class join))
         if join-clause
           do (write-char #\space out)
              (write-string join-clause out))))

(defun build-where (query-builder out)
  (let ((wheres (query-builder-where query-builder)))
    (when wheres
      (format
       out " where ~{~a~^ and ~}"
       (loop for where in wheres
             if (null (cdr where))
               append where
             else
               append
               (loop for (col val) on where by #'cddr
                     collect (cond ((typep val 'operator)
                                    (format nil "~/dbq::col/ ~a ~/dbq::val/"
                                            col
                                            (operator-of val)
                                            (operand-of val)))
                                   ((or (consp val)
                                        (query-builder-p val))
                                    (format nil "~/dbq::col/ in ~/dbq::val/" col val))
                                   (t
                                    (format nil "~/dbq::col/ = ~/dbq::val/" col val)))))))))

(defun build-group (query-builder out)
  (let ((group (query-builder-group query-builder)))
    (when group
      (format out " group by ~{~/dbq::col/~^ ,~}" (alexandria:ensure-list group)))))

(defmacro query (query-builder &body body)
  `(let* ((*query-builder* t)
          (*query-builder* (to-query-builder ,query-builder)))
     (declare (special *query-builder*))
     ,@body
     *query-builder*))

(defun set-value (object value column)
  (let ((slot (find (to-column-name column) (sb-mop:class-slots (class-of object))
                    :test #'string-equal
                    :key (lambda (x) (to-column-name (sb-mop:slot-definition-name x))))))
    (if slot
        (setf (slot-value object (sb-mop:slot-definition-name slot))
              value)
        (warn "no slot for column: ~a value: ~a!" column value))))

(defun store (class rows)
  (loop for alist in rows
        collect (let ((object (make-instance class)))
                  (loop for (column . value) in alist
                        do (set-value object value column))
                  object)))

(defun fetch-one (query &key (class (query-builder-from query)))
  (car (store class (execute (sql (query query (limit 1)))))))

(defun fetch (query &key (class (query-builder-from query)))
  (let ((results (store class (execute (sql query)))))
    (when (and results (query-builder-preload query))
      (preload-has-many results query)
      (preload-hbtm results query)
      (preload-belongs-to results query))
    results))

(defun find-by (class &rest conditions)
  (let ((query (query class (apply #'where conditions))))
   (fetch-one query :class (query-builder-from query))))

(defun count (query)
  (cdaar (execute (count-sql query))))
