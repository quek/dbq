(in-package :dbq)


(defun establish-connection (&key (host "localhost") user password database (port 5432))
  (disconnect)
  (postmodern:connect-toplevel database user password host :port port))

(defun disconnect ()
  (postmodern:disconnect-toplevel))

(defmacro with-connection  ((&key (host "localhost") user password database (port 5432)) &body body)
  `(sb-sys:without-interrupts
     (let* ((postmodern:*database* nil)
            (postmodern:*database*
              (sb-sys:allow-with-interrupts
                (postmodern:connect ,database ,user ,password ,host :port ,port))))
       (unwind-protect
            (sb-sys:with-local-interrupts
              ,@body)
         (when postmodern:*database*
           (postmodern:disconnect postmodern:*database*))))))

;; 9時間ずれた universal-time でかえってくるので
(let ((offset (* 60 60
                 (car (last (multiple-value-list
                             (decode-universal-time (get-universal-time))))))))
  (flet ((timestamp-reader (useconds-since-2000)
           (multiple-value-bind (quotient remainder) (floor useconds-since-2000 1000000)
             (let ((timestamp (local-time:universal-to-timestamp (+ cl-postgres::+start-of-2000+ quotient
                                                                    offset))))
               (local-time:timestamp+ timestamp (* remainder 1000) :nsec))))
         (date-reader (days-since-2000)
           (local-time:universal-to-timestamp
            (+ cl-postgres::+start-of-2000+
               (* days-since-2000 cl-postgres::+seconds-in-day+)
               offset))))

    (cl-postgres:set-sql-datetime-readers :timestamp #'timestamp-reader
                                          :date #'date-reader)))

(defun execute (sql)
  (log4cl:log-debug sql)
  (postmodern:query sql :str-alists))

(define-condition rollback-error (error) ())

(defun rollback ()
  (error 'rollback-error))

(defmacro with-transaction (&body body)
  (let ((statement (gensym)))
    `(let ((,statement "rollback"))
       (execute "begin")
       (unwind-protect
            (handler-case (prog1 (progn ,@body)
                            (setf ,statement "commit"))
              (rollback-error ()))
         (execute ,statement)))))
