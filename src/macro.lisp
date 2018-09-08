(in-package :dbq)

(defvar *query-builder* nil)

(defmacro query (query-builder &body body)
  `(let* ((*query-builder* t)
          (*query-builder* (to-query-builder ,query-builder)))
     (declare (special *query-builder*))
     ,@body
     *query-builder*))
