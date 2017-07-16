(in-package :dbq)

(defvar *belongs-to* (make-hash-table :test #'eq))

(defmacro def-belongs-to (&key class slot (other-class slot)
                            (foreign-key-slot (sym other-class "-id"))
                            (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.id=~/dbq::tbl/.~/dbq::col/"
                                                 other-class other-class class foreign-key-slot)))
  `(progn
     (setf (gethash ',slot
                    (or (gethash ',class *belongs-to*)
                        (setf (gethash ',class *belongs-to*) (make-hash-table :test #'eq))))
           '(:other-class ,other-class
             :foreign-key-slot ,foreign-key-slot
             :join-clause ,join-clause))
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (setf (slot-value instance slot-name)
             (aif (slot-value instance ',foreign-key-slot)
                  (find-by ',other-class :id it))))))

(defun belongs-to-config (class slot key)
  (awhen (gethash class *belongs-to*)
    (getf (gethash slot it) key)))

(defun belongs-to-join-clause (class slot)
  (belongs-to-config class slot :join-clause))

(defun belongs-to-slot-p (record slot)
  (aand  (gethash (class-name (class-of record)) *belongs-to*)
         (gethash slot it)))
