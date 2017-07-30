(in-package :dbq)

(defun val (stream value &rest args)
  (declare (ignore args))
  (write-string (to-sql-value value) stream))

(defun col (stream value &rest args)
  (declare (ignore args))
  (etypecase value
    (string
     (write-string value stream))
    (symbol
     (write-char #\" stream)
     (loop for c across (to-column-name value)
           do (cond ((char= #\. c)
                     (write-string "\".\"" stream))
                    ((char= #\" c)
                     (write-string "\"\"" stream))
                    (t
                     (write-char c stream))))
     (write-char #\" stream))))

(defun tbl (stream value &rest args)
  (declare (ignore args))
  (write-char #\" stream)
  (loop for c across (to-table-name value)
        if (char= #\" c)
          do (write-char #\" stream)
        do (write-char c stream))
  (write-char #\" stream))
