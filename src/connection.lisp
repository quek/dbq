(in-package :dbq)


(defun establish-connection (&key (host "localhost") user password database port)
  (cl-mysql:connect :host host :user user :password password :database database :port port))

(defun disconnect ()
  (cl-mysql:disconnect))

(defmacro with-connection  ((&key (host "localhost") user password database port) &body body)
  `(progn
     (establish-connection :host ,host :user ,user :password ,password :database ,database :port ,port)
     (unwind-protect
          (progn ,@body)
       (disconnect))))

;; 9時間ずれた universal-time でかえってくるので
(let ((offset (- (* 60 60
                    (car (last (multiple-value-list
                                (decode-universal-time (get-universal-time)))))))))
  (defun datetime-string-to-local-time (string &optional len)
    (declare (ignore len))
    (if (equal "0000-00-00 00:00:00" string)
        nil
        (local-time:parse-timestring string :date-time-separator #\space :offset offset))))

(setf (gethash :datetime cl-mysql:*type-map*) 'datetime-string-to-local-time)
(setf (gethash :timestamp cl-mysql:*type-map*) 'datetime-string-to-local-time)

(defun execute (sql)
  (print sql)
  (cl-mysql:query sql))
