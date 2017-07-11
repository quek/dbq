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

(defun has-many-slot-p (record slot)
  (aand  (gethash (class-name (class-of record)) *has-many*)
         (gethash slot it)))

(defun has-many-slots (class)
  (aif (gethash class *has-many*)
       (loop for slot being the hash-keys of it
             collect slot)))

(defun has-many-config (class slot key)
  (awhen (gethash class *has-many*)
    (getf (gethash slot it) key)))

(defun has-many-join-clause (class slot)
  (has-many-config class slot :join-clause))

(defun has-many-other-class (class slot)
  (has-many-config class slot :other-class))
