(in-package :dbq)

(defvar *hbtm* (make-hash-table :test #'eq))

(defmacro def-hbtm (&key class slot other-class table
                      (join-clause (format nil "inner join ~a on ~a.~a_id=~a.id inner join ~a on ~a.id = ~a.~a_id"
                                           table table (to-column-name class) (to-table-name class)
                                           (to-table-name other-class) (to-table-name other-class)
                                           table (to-column-name other-class))))
  `(progn
     (setf (gethash ',slot
                    (or (gethash ',class *hbtm*)
                        (setf (gethash ',class *hbtm*) (make-hash-table :test #'eq))))
           '(:join-clause ,join-clause :other-class ,other-class :table ,table))
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (fetch (query ',class
                (select ,(str "distinct " (to-column-name slot) ".*"))
                (join ',slot) (where ,(str (to-table-name class) ".id") (id-of instance)))
              :class ',other-class))))

(defun hbtm-slots (class)
  (aif (gethash class *hbtm*)
       (loop for slot being the hash-keys of it
             collect slot)))

(defun hbtm-config (class slot key)
  (awhen (gethash class *hbtm*)
    (getf (gethash slot it) key)))

(defun hbtm-join-clause (class slot)
  (hbtm-config class slot :join-clause))

(defun hbtm-table (class slot)
    (hbtm-config class slot :table))

(defun hbtm-other-class (class slot)
  (hbtm-config class slot :other-class))

(defun insert-hbtm (record)
  (loop with class = (class-name (class-of record))
        for slot in (hbtm-slots class)
        if (slot-boundp record slot)
          do (execute (format nil "delete from ~a where ~a_id=~d"
                              (hbtm-table class slot)
                              (to-column-name class)
                              (to-sql-value (id-of record))))
             (loop for x in (slot-value record slot)
                   unless (persistedp x)
                     do (save x)
                        (execute (format nil "insert into ~a (~a_id, ~a_id) values(~d, ~d)"
                                         (hbtm-table class slot)
                                         (to-column-name class)
                                         (to-column-name (hbtm-other-class class slot))
                                         (to-sql-value (id-of record))
                                         (to-sql-value (id-of x)))))))

