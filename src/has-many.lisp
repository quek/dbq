(in-package :dbq)

(defvar *has-many* (make-hash-table :test #'eq))

(defmacro def-has-many (&key class slot
                          (other-class (sym (singularize slot)))
                          (foreign-key-slot (sym class "-id"))
                          (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.id"
                                               other-class other-class foreign-key-slot class)))
  `(progn
     (setf (gethash ',slot
                    (or (gethash ',class *has-many*)
                        (setf (gethash ',class *has-many*) (make-hash-table :test #'eq))))
           '(:other-class ,other-class
             :foreign-key-slot ,foreign-key-slot
             :join-clause ,join-clause))
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (setf (slot-value instance slot-name)
             (if (persistedp instance)
                 (fetch (query ',other-class
                          (where ',foreign-key-slot (id-of instance)))))))))

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

(defun update-has-many (record)
  "新しいリストにないものは削除してみよう。
問題があるようなら _delete スロット追加とか検討する。"
  (loop with id = (id-of record)
        with class = (class-name (class-of record))
        for slot in (has-many-slots class)
        for other-class = (has-many-other-class class slot)
        for foreign-key-slot = (has-many-foreign-key-slot class slot)
        if (slot-boundp record slot)
          do (let ((old-list (fetch (query other-class (where foreign-key-slot id)))))
              (loop for x in (slot-value record slot)
                    do (setf (slot-value x (has-many-foreign-key-slot class slot))
                             id)
                       (save x)
                       (setf old-list (cl:delete x old-list :test #'id=)))
               (loop for old in old-list
                     do (delete old)))))
