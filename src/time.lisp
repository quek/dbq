(in-package :dbq)

(defclass time ()
  ((hour :initarg :hour :initform 0 :accessor .hour :type (mod 24))
   (minute :initarg :minute :initform 0 :accessor .minute :type (mod 60))
   (second :initarg :second :initform 0 :accessor .second :type (mod 60))
   (microsecond :initarg :microsecond :initform 0 :accessor .microsecond :type (mod 1000000))))

(defmethod print-object ((time time) stream)
  (format stream "~2,'0d:~2,'0d:~2,'0d.~6,'0d"
          (.hour time) (.minute time) (.second time) (.microsecond time)))

(defmethod to-sql-value ((time time))
  (to-sql-value (princ-to-string time)))

(defun time-from-string (string)
  (let ((x (mapcar #'parse-integer
                  (ppcre:split "[^\\d]" string))))
    (make-instance 'time :hour (or (car x) 0)
                         :minute (or (cadr x) 0)
                         :second (or (caddr x) 0)
                         :microsecond (or (cadddr x) 0))))
