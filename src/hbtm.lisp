(in-package :dbq)

(defvar *hbtm* (make-hash-table :test #'equal))

(defmacro def-hbtm (&key class slot join-clause)
  `(setf (gethash '(,class ,slot) *hbtm*) ,join-clause))

(defun hbtm-join-clause (class slot)
  (gethash (list class slot) *hbtm*))


