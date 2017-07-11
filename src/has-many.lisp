(in-package :dbq)

(defvar *has-many* (make-hash-table :test #'eq))

(defmacro def-has-many (&key class slot other-class
                          (foreign-key (str (to-column-name class) "_id"))
                          (join-clause (format nil "inner join ~a on ~a.~a=~a.idn"
                                               (to-table-name other-class)
                                               (to-table-name other-class)
                                               foreign-key
                                               (to-table-name class))))
  `(progn
     (setf (gethash ',slot
                    (or (gethash ',class *has-many*)
                        (setf (gethash ',class *has-many*) (make-hash-table :test #'eq))))
           '(:other-class ,other-class
             :foreign-ket ,foreign-key
             :join-clause ,join-clause))
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (fetch (query ',other-class
                (where ,foreign-key (id-of instance)))))))
