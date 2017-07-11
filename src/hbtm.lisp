(in-package :dbq)

(defvar *hbtm* (make-hash-table :test #'eq))

(defmacro def-hbtm (&key class slot other-class table
                      (join-clause
                       (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.id ~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.id = ~/dbq::tbl/.~/dbq::col/"
                        table
                        table (str (to-column-name class) "_id") class
                        other-class other-class
                        table (str (to-column-name other-class) "_id"))))
  `(progn
     (setf (gethash ',slot
                    (or (gethash ',class *hbtm*)
                        (setf (gethash ',class *hbtm*) (make-hash-table :test #'eq))))
           '(:join-clause ,join-clause :other-class ,other-class :table ,table))
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (fetch (query ',class
                (select ,(format nil "distinct ~/dbq::tbl/.*" other-class))
                (join ',slot) (where ,(format nil "~/dbq::tbl/.id"  class) (id-of instance)))
              :class ',other-class))))

(defun hbtm-slot-p (record slot)
  (aand  (gethash (class-name (class-of record)) *hbtm*)
         (gethash slot it)))

(defun hbtm-slots (class)
  (aif (gethash class *hbtm*)
       (loop for slot being the hash-keys of it
             collect slot)))

(defun hbtm-config (class slot key)
  (awhen (gethash class *hbtm*)
    (getf (gethash slot it) key)))

(defun hbtm-join-clause (class slot)
  (hbtm-config class slot :join-clause))

(defun hbtm-table (class slot)
    (hbtm-config class slot :table))

(defun hbtm-other-class (class slot)
  (hbtm-config class slot :other-class))

(defun update-hbtm (record)
  (loop with class = (class-name (class-of record))
        for slot in (hbtm-slots class)
        if (slot-boundp record slot)
          do (execute (format nil "delete from ~/dbq::tbl/ where ~/dbq::col/=~/dbq::val/"
                              (hbtm-table class slot)
                              (str (to-column-name class) "_id")
                              (id-of record)))
             (loop for x in (slot-value record slot)
                   unless (persistedp x)
                     do (save x)
                        (execute (format nil "~
insert into ~/dbq::tbl/ (~/dbq::col/, ~/dbq::col/) values(~/dbq::val/, ~/dbq::val/)"
                                         (hbtm-table class slot)
                                         (to-foreign-key-column class)
                                         (to-foreign-key-column (hbtm-other-class class slot))
                                         (id-of record)
                                         (id-of x))))))

