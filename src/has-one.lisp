(in-package :dbq)

(defvar *has-one* (make-hash-table :test #'eq))

(flet ((make-join-clause (class through slot foreign-key-slot other-class)
         (if through
             (progn
               (format nil "~a ~a"
                       (or (has-one-join-clause class through)
                           (belongs-to-join-clause class through))
                       (or (has-one-join-clause (sym (singularize through)) slot)
                           (belongs-to-join-clause (sym (singularize through)) slot))))
             (format nil "inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.id"
                     other-class other-class foreign-key-slot class)))
       (make-through-query (class through other-class foreign-key-slot)
         (let ((through-class (if (has-one-slot-p class through)
                                  (sym (singularize through))
                                  through)))
           `(query ',other-class
              (select ,(format nil "distinct ~/dbq::tbl/.*" other-class))
              (join ',through)
              (where ,(format nil "~/dbq::tbl/.~/dbq::col/" through-class foreign-key-slot)
                     (id-of instance))))))

  (defmacro def-has-one (&key class slot
                           through
                           (other-class slot)
                           (foreign-key-slot (column-name-to-slot-name (to-foreign-key-column class)))
                           (join-clause (make-join-clause class through slot foreign-key-slot other-class)))
    `(progn
       (setf (gethash ',slot
                      (or (gethash ',class *has-one*)
                          (setf (gethash ',class *has-one*) (make-hash-table :test #'eq))))
             '(:through ,through
               :other-class ,other-class
               :foreign-key-slot ,foreign-key-slot
               :join-clause ,join-clause))
       (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
         (if *query-builder*
             (setf *query-builder*
                   ,(if through
                        (make-through-query class through other-class foreign-key-slot)
                        `(query ',other-class
                           (where ',foreign-key-slot (id-of instance)))))
             (setf (slot-value instance slot-name)
                   (if (persistedp instance)
                       (fetch-one (query ',other-class
                                    (where ',foreign-key-slot (id-of instance)))))))))))

(defgeneric has-one-slot-p (x slot)
  (:method ((class-name symbol) slot)
    (aand  (gethash class-name *has-one*)
           (gethash slot it)))
  (:method (record slot)
    (has-one-slot-p (class-name (class-of record)) slot)))

(defun has-one-slots (class)
  (aif (gethash class *has-one*)
       (loop for slot being the hash-keys of it
             collect slot)))

(defun has-one-config (class slot key)
  (awhen (gethash class *has-one*)
    (getf (gethash slot it) key)))

(defun has-one-join-clause (class slot)
  (has-one-config class slot :join-clause))

(defun has-one-foreign-key-slot (class slot)
  (has-one-config class slot :foreign-key-slot))

(defun has-one-other-class (class slot)
  (has-one-config class slot :other-class))

(defun has-one-through (class slot)
  (has-one-config class slot :through))

(defun preload-has-one (records query)
  (loop with class = (query-builder-from query)
        with ids = (delete-duplicates (mapcar #'id-of records))
        for slot in (intersection (query-builder-preload query)
                                  (has-one-slots class))
        for other-class = (has-one-other-class class slot)
        for foreign-key-slot = (has-one-foreign-key-slot class slot)
        for children = (fetch (query other-class (where foreign-key-slot ids)))
        do (loop for record in records
                 do (setf (slot-value record slot) nil))
           (loop for child in (nreverse children)
                 for record = (find (slot-value child foreign-key-slot)
                                    records :key #'id-of)
                 do (setf (slot-value record slot) child))))

