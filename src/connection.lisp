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

(defun execute (sql)
  (print sql)
  (cl-mysql:query sql))
