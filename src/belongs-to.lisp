(in-package :dbq)

(defvar *belongs-to* (make-hash-table :test #'eq))

(defmacro def-belongs-to (&key class slot (other-class slot)
                            (foreign-key-slot (sym slot "-ID"))
                            (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.id=~/dbq::tbl/.~/dbq::col/"
                                                 other-class other-class class foreign-key-slot))
                            (writer `(setf ,(sym other-class "-OF"))))
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
                  (find-by ',other-class :id it))))
     (defmethod ,writer :after (value (instance ,class))
       (setf (slot-value instance ',foreign-key-slot) (dbq::id-of value)))))

(defun belongs-to-config (class slot key)
  (awhen (gethash class *belongs-to*)
    (getf (gethash slot it) key)))

(defun belongs-to-foreign-key-slot (class slot)
  (belongs-to-config class slot :foreign-key-slot))

(defun belongs-to-join-clause (class slot)
  (belongs-to-config class slot :join-clause))

(defun belongs-to-other-class (class slot)
  (belongs-to-config class slot :other-class))

(defun belongs-to-slots (class)
  (aif (gethash class *belongs-to*)
       (loop for slot being the hash-keys of it
             collect slot)))

(defgeneric belongs-to-slot-p (x slot)
  (:method ((class-name symbol) slot)
    (aand  (gethash class-name *belongs-to*)
           (gethash slot it)))
  (:method (record slot)
    (belongs-to-slot-p (class-name (class-of record)) slot)))

(defun preload-belongs-to (records query)
  (when records
    (loop with class = (class-name (class-of (car records))) ;STI で動かないよね・・・
          for slot in (intersection (query-builder-preload query)
                                    (belongs-to-slots class))
          for other-class = (belongs-to-other-class class slot)
          for foreign-key-slot = (belongs-to-foreign-key-slot class slot)
          for ids = (delete-duplicates (mapcar (lambda (b) (slot-value b foreign-key-slot)) records)
                                       :test #'equal)
          for parents = (fetch (query other-class (where :id ids)))
          do (loop for record in records
                   for parent = (find (slot-value record foreign-key-slot) parents :key #'id-of
                                                                                   :test #'equal)
                   do (setf (slot-value record slot) parent)))))
