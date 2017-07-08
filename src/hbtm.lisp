(in-package :dbq)

(defvar *hbtm* (make-hash-table :test #'equal))

(defmacro def-hbtm (&key class slot join-clause other-class)
  `(progn
     (setf (gethash '(,class ,slot) *hbtm*) ,join-clause)
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (fetch (query ',class
                (select ,(str "distinct " (string-downcase slot) ".*"))
                (join ',slot) (where ,(str (to-table-name class) ".id") (id-of instance)))
              :class ',other-class))))


(defun hbtm-join-clause (class slot)
  (gethash (list class slot) *hbtm*))


