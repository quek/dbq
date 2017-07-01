(in-package :dbq)


(defun establish-connection (&key (host "localhost") user password database port)
  (cl-mysql:connect :host host :user user :password password :database database :port port))

(defun execute (sql)
  (cl-mysql:query sql))
