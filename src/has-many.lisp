(in-package :dbq)

(defvar *has-many* (make-hash-table :test #'eq))

(flet ((make-join-clause (class through slot foreign-key-slot other-class)
         (if through
             (progn
               (format nil "~a ~a"
                       (has-many-join-clause class through)
                       (has-many-join-clause (sym (singularize through)) slot)))
             (format nil "inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.id"
                     other-class other-class foreign-key-slot class))))

  (defmacro def-has-many (&key class slot
                            through
                            (other-class (sym (singularize slot)))
                            (foreign-key-slot (sym class "-id"))
                            (join-clause (make-join-clause class through slot foreign-key-slot other-class))
                            (order (format nil "~/dbq::tbl/.id" other-class)))
    `(progn
       (setf (gethash ',slot
                      (or (gethash ',class *has-many*)
                          (setf (gethash ',class *has-many*) (make-hash-table :test #'eq))))
             '(:through ,through
               :other-class ,other-class
               :foreign-key-slot ,foreign-key-slot
               :join-clause ,join-clause))
       (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
         (if *query-builder*
             (setf *query-builder*
                   ,(if through
                        `(query ',other-class
                           (select ,(format nil "distinct ~/dbq::tbl/.*" other-class))
                           (from ',(sym (singularize through)))
                           (join ',slot)
                           (where ',(has-many-foreign-key-slot class through)
                                  (id-of instance)))
                        `(query ',other-class
                           (where ',foreign-key-slot (id-of instance)))))
             (setf (slot-value instance slot-name)
                   (if (persistedp instance)
                       (fetch (query ',other-class
                                (where ',foreign-key-slot (id-of instance))
                                (order ,order))))))))))

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

(defun has-many-foreign-key-slot (class slot)
  (has-many-config class slot :foreign-key-slot))

(defun has-many-other-class (class slot)
  (has-many-config class slot :other-class))

(defun has-many-through (class slot)
  (has-many-config class slot :through))

(defun update-has-many (record)
  "新しいリストにないものは削除してみよう。
問題があるようなら _delete スロット追加とか検討する。"
  (loop with id = (id-of record)
        with class = (class-name (class-of record))
        for slot in (has-many-slots class)
        for other-class = (has-many-other-class class slot)
        for foreign-key-slot = (has-many-foreign-key-slot class slot)
        for through = (has-many-through class slot)
        if (and (null through) (slot-boundp record slot))
          do (let ((old-list (fetch (query other-class (where foreign-key-slot id)))))
              (loop for x in (slot-value record slot)
                    do (setf (slot-value x (has-many-foreign-key-slot class slot))
                             id)
                       (save x)
                       (setf old-list (cl:delete x old-list :test #'id=)))
               (loop for old in old-list
                     do (delete old)))))

(defun preload-has-many (records query)
  (loop with class = (query-builder-from query)
        with ids = (delete-duplicates (mapcar #'id-of records))
        for slot in (intersection (query-builder-preload query)
                                  (has-many-slots class))
        for other-class = (has-many-other-class class slot)
        for foreign-key-slot = (has-many-foreign-key-slot class slot)
        for children = (fetch (query other-class (where foreign-key-slot ids)))
        do (loop for record in records
                 do (setf (slot-value record slot) nil))
           (loop for child in (nreverse children)
                 for record = (find (slot-value child foreign-key-slot)
                                    records :key #'id-of)
                 do (push child (slot-value record slot)))))

