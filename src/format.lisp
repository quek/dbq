(in-package :dbq)

(defun val (stream value &rest args)
  (declare (ignore args))
  (write-string (to-sql-value value) stream))

(defun col (stream value &rest args)
  (declare (ignore args))
  (write-char #\` stream)
  (loop for c across (to-column-name value)
        if (char= #\` c)
          do (write-char #\` stream)
        do (write-char c stream))
  (write-char #\` stream))

(defun tbl (stream value &rest args)
  (declare (ignore args))
  (write-char #\` stream)
  (loop for c across (to-table-name value)
        if (char= #\` c)
          do (write-char #\` stream)
        do (write-char c stream))
  (write-char #\` stream))

(assert
 (string= "select from `dao_mixins` where `ab``cd`='ab'';c' and `a_b_c`=123"
          (format nil
                  "select from ~/dbq::tbl/ where ~/dbq::col/=~/dbq::val/ and ~/dbq::col/=~/dbq::val/"
                  'dao-mixin "ab`cd" "ab';c" 'a-b-c 123)))
