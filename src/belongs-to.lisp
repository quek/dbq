(in-package :dbq)

(defvar *belongs-to* (make-hash-table :test #'eq))

(defmacro def-belongs-to (&key class other-class
                            (method (sym other-class "-of"))
                            (foreign-key-slot (sym other-class "-id"))
                            (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.id=~/dbq::tbl/.~/dbq::col/"
                                                 other-class other-class class foreign-key-slot)))
  `(progn
     (setf (gethash ',other-class
                    (or (gethash ',class *belongs-to*)
                        (setf (gethash ',class *belongs-to*) (make-hash-table :test #'eq))))
           '(:other-class ,other-class
             :foreign-key-slot ,foreign-key-slot
             :join-clause ,join-clause))
     ;; TODO slot にする！！！
     (defmethod ,method ((record ,class))
       (aif (slot-value record ',foreign-key-slot)
            (find-by ',other-class :id it)))
     (defmethod (setf ,method) (value (record ,class))
       (setf (slot-value record ',foreign-key-slot) (id-of value))
       value)))

(defun belongs-to-config (class name key)
  (awhen (gethash class *belongs-to*)
    (getf (gethash name it) key)))

(defun belongs-to-join-clause (class name)
  (belongs-to-config class name :join-clause))
