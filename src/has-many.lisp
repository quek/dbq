(in-package :dbq)

(defvar *has-many* (make-hash-table :test #'eq))

(flet ((make-join-clause (class through slot foreign-key-slot other-class)
         (if through
             (progn
               (format nil "~a ~a"
                       (has-many-join-clause class through)
                       (has-many-join-clause (sym (singularize through)) slot)))
             (format nil "inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.id"
                     other-class other-class foreign-key-slot class)))
       (make-through-query (class slot through other-class foreign-key-slot)
         (let ((through-class (if (has-many-slot-p class through)
                                  (sym (singularize through))
                                  through)))
           `(query ',other-class
              (select ,(format nil "distinct ~/dbq::tbl/.*" other-class))
              (join ',through)
              #+nil
              (join ',(cond ((has-many-slot-p through-class slot)
                             slot)
                            ((belongs-to-slot-p through-class other-class)
                             other-class)
                            (t
                             (error "なんかだめです ~a ~a ~a ~a ~a"
                                    class through through-class slot other-class))))
              (where ,(format nil "~/dbq::tbl/.~/dbq::col/" through-class foreign-key-slot)
                     (id-of instance))))))

  (defmacro def-has-many (&key class slot
                            through
                            (other-class (sym (singularize slot)))
                            (foreign-key-slot (column-name-to-slot-name (to-foreign-key-column class)))
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
                        (make-through-query class slot through other-class foreign-key-slot)
                        `(query ',other-class
                           (where ',foreign-key-slot (id-of instance)))))
             (setf (slot-value instance slot-name)
                   (if (persistedp instance)
                       (fetch (query ',other-class
                                (where ',foreign-key-slot (id-of instance))
                                (order ,order))))))))))

(defgeneric has-many-slot-p (x slot)
  (:method ((class-name symbol) slot)
    (aand  (gethash class-name *has-many*)
           (gethash slot it)))
  (:method (record slot)
    (has-many-slot-p (class-name (class-of record)) slot)))

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

