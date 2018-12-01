(in-package :dbq)

(defvar *reldat* (make-hash-table :test #'eq))

(defclass reldat ()
  ((class :initarg :class)
   (slot :initarg :slot)
   (other-class :initarg :other-class)
   (join-clause :initarg :join-clause)))

(defclass reldat-foreign-key-mixin ()
  ((primary-key :initarg :primary-key)
   (foreign-key :initarg :foreign-key)))

(defclass reldat-hbtm (reldat reldat-foreign-key-mixin)
  ((table :initarg :table)
   (other-primary-key :initarg :other-primary-key)
   (other-foreign-key :initarg :other-foreign-key)))

(defclass reldat-through-mixin ()
  ((through :initarg :through)))

(defclass reldat-has-many (reldat reldat-foreign-key-mixin reldat-through-mixin)
  ((order :initarg :order)))

(defclass reldat-has-one (reldat reldat-foreign-key-mixin reldat-through-mixin)
  ())

(defclass reldat-belongs-to (reldat reldat-foreign-key-mixin)
  ())

(defmethod reverse-join-clause ((reldat reldat-through-mixin))
  (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
          (slot-value reldat 'class)
          (slot-value reldat 'other-class) (slot-value reldat 'foreign-key)
          (slot-value reldat 'class) (slot-value reldat 'primary-key) ))

(defmethod reverse-join-clause ((reldat reldat-belongs-to))
  (format nil "inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
          (slot-value reldat 'class)
          (slot-value reldat 'other-class) (slot-value reldat 'primary-key)
          (slot-value reldat 'class) (slot-value reldat 'foreign-key) ))

(defmethod reverse-join-clause ((reldat reldat-hbtm))
  (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/ ~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/ ~
"
          (slot-value reldat 'table)
          (slot-value reldat 'table) (slot-value reldat 'other-foreign-key)
          (slot-value reldat 'other-class) (slot-value reldat 'other-primary-key)
          (slot-value reldat 'class)
          (slot-value reldat 'class) (slot-value reldat 'primary-key)
          (slot-value reldat 'table) (slot-value reldat 'foreign-key)))

(defmethod initialize-instance :after ((reldat reldat) &key class slot &allow-other-keys)
  (sif (gethash class *reldat*)
       (setf (gethash slot it) reldat)
       (progn (setf it (make-hash-table :test #'eq))
              (setf (gethash slot it) reldat))))

(defun reldat (class slot)
  (awhen (gethash class *reldat*)
    (gethash slot it)))

(flet ((f (class target)
         (aif (gethash class *reldat*)
              (loop for slot being the hash-keys of it
                      using (hash-value reldat)
                    if (typep reldat target)
                      collect slot))))
  (defun has-many-slots (class)
    (f class 'reldat-has-many))

  (defun has-one-slots (class)
    (f class 'reldat-has-one))

  (defun belongs-to-slots (class)
    (f class 'reldat-belongs-to))

  (defun hbtm-slots (class)
    (f class 'reldat-hbtm)))

(defmacro define-has-many (class slot
                           &key (other-class (sym (singularize slot)))
                             through
                             (primary-key 'id)
                             foreign-key
                             join-clause
                             order)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((foreign-key ,(or foreign-key
                              (if through
                                  `(to-foreign-key (slot-value (reldat ',class ',through) 'other-class))
                                  `(to-foreign-key ',class))))
            (join-clause ,(or join-clause
                              (if through
                                  `(let* ((reldat (reldat ',class ',through))
                                          (through-class (slot-value reldat 'other-class))
                                          (through-reldat
                                            (or (reldat through-class ',slot)
                                                (reldat through-class (sym (singularize ',slot))))))
                                     (format nil "~a ~a"
                                             (slot-value reldat 'join-clause)
                                             (slot-value through-reldat 'join-clause)))
                                  `(format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                           ',other-class ',other-class foreign-key
                                           ',class ',primary-key)))))
       (make-instance
        'reldat-has-many
        :class ',class
        :slot ',slot
        :other-class ',other-class
        :foreign-key foreign-key
        :primary-key ',primary-key
        :through ',through
        :join-clause join-clause
        :order ,order)

       (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
         (if *query-builder*
             (setf *query-builder*
                   ,(if through
                        (let* ((reldat (reldat class through))
                               (through-class (slot-value reldat 'other-class))
                               (through-reldat (or (reldat through-class slot)
                                                   (reldat through-class (sym (singularize slot))))))
                          ;; TODO ここ再帰にしないどだめ
                          (if (and (slot-exists-p reldat 'through)
                                   (slot-boundp reldat 'through)
                                   (slot-value reldat 'through))
                              (let* ((x (reldat class (slot-value reldat 'through)))
                                     (y (reldat (slot-value x 'other-class) through)))
                                `(query ',other-class
                                   (join ,(reverse-join-clause through-reldat))
                                   (join ,(reverse-join-clause y))
                                   (where ',(slot-value x 'foreign-key) (.id instance))))
                              `(query ',other-class
                                 (join ,(reverse-join-clause through-reldat))
                                 (where ',(slot-value reldat 'foreign-key) (.id instance)))))
                        `(query ',other-class
                           (where foreign-key (.id instance)))))
             (setf (slot-value instance slot-name)
                   (if (persistedp instance)
                       ,(if through
                            (let* ((reldat (reldat class through))
                                   (through-class (slot-value reldat 'other-class))
                                   (through-reldat (or (reldat through-class slot)
                                                       (reldat through-class (sym (singularize slot))))))
                              ;; TODO ここ再帰にしないどだめ
                              (if (and (slot-exists-p reldat 'through)
                                       (slot-boundp reldat 'through)
                                       (slot-value reldat 'through))
                                  (let* ((x (reldat class (slot-value reldat 'through)))
                                         (y (reldat (slot-value x 'other-class) through)))
                                    `(fetch (query ',other-class
                                              (join ,(reverse-join-clause through-reldat))
                                              (join ,(reverse-join-clause y))
                                              (where ',(slot-value x 'foreign-key) (.id instance))
                                              ,@(when order `((order ,order))))))
                                  `(fetch (query ',other-class
                                            (join ,(reverse-join-clause through-reldat))
                                            (where ',(slot-value reldat 'foreign-key) (.id instance))
                                            ,@(when order `((order ,order)))))))
                            `(fetch (query ',other-class
                                      (where foreign-key (.id instance))
                                      ,@(when order `((order ,order)))))))))))))

(defmacro define-has-one (class slot
                          &key (other-class (sym (singularize slot)))
                            through
                            (primary-key 'id)
                            foreign-key
                            join-clause
                            order)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((foreign-key ,(or foreign-key
                              (if through
                                  `(to-foreign-key (slot-value (reldat ',class ',through) 'other-class))
                                  `(to-foreign-key ',class))))
            (join-clause ,(or join-clause
                              (if through
                                  `(let* ((reldat (reldat ',class ',through))
                                          (through-class (slot-value reldat 'other-class))
                                          (through-reldat
                                            (or (reldat through-class ',slot)
                                                (reldat through-class (sym (singularize ',slot))))))
                                     (format nil "~a ~a"
                                             (slot-value reldat 'join-clause)
                                             (slot-value through-reldat 'join-clause)))
                                  `(format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                           ',other-class ',other-class foreign-key
                                           ',class ',primary-key)))))
       (make-instance 'reldat-has-one
                      :class ',class
                      :slot ',slot
                      :other-class ',other-class
                      :foreign-key foreign-key
                      :primary-key ',primary-key
                      :through ',through
                      :join-clause join-clause)
       (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
         (if *query-builder*
             (setf *query-builder*
                   ,(if through
                        (let* ((reldat (reldat class through))
                               (through-class (slot-value reldat 'other-class))
                               (through-reldat (or (reldat through-class slot)
                                                   (reldat through-class (sym (singularize slot))))))
                          `(query ',other-class
                             (join ,(reverse-join-clause through-reldat))
                             (where ',(slot-value reldat 'foreign-key) (.id instance))))
                        `(query ',other-class
                           (where foreign-key (.id instance)))))
             (setf (slot-value instance slot-name)
                   (if (persistedp instance)
                       ,(if through
                            (let* ((reldat (reldat class through))
                                   (through-class (slot-value reldat 'other-class))
                                   (through-reldat (or (reldat through-class slot)
                                                       (reldat through-class (sym (singularize slot))))))
                              `(fetch-one (query ',other-class
                                            (join ,(reverse-join-clause through-reldat))
                                            (where ',(slot-value reldat 'foreign-key) (.id instance))
                                            ,@(when order `((order ,order))))))
                            `(fetch-one (query ',other-class
                                          (where foreign-key (.id instance))
                                          ,@(when order `((order ,order)))))))))))))

(defmacro define-belongs-to
    (class slot
     &key (other-class slot)
       (primary-key 'id)
       (foreign-key (to-foreign-key slot))
       (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                            other-class other-class primary-key class foreign-key ))
       (writer `(setf ,(sym "." other-class))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'reldat-belongs-to
                    :class ',class
                    :slot ',slot
                    :other-class ',other-class
                    :foreign-key ',foreign-key
                    :primary-key ',primary-key
                    :join-clause ,join-clause)
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (if *query-builder*
           (setf *query-builder*
                 (query ',other-class
                   (where ',primary-key (slot-value instance ',foreign-key))))
           (setf (slot-value instance slot-name)
                 (if (persistedp instance)
                     (fetch-one (query ',other-class
                                  (where ',primary-key (slot-value instance ',foreign-key))
                                  (limit 1)))))))
     (defmethod ,writer :after (value (instance ,class))
       (setf (slot-value instance ',foreign-key) (slot-value value ',primary-key)))))

(defmacro define-hbtm
    (class slot table
     &key
       (other-class (sym (singularize slot)))
       (primary-key 'id)
       (foreign-key (to-foreign-key class))
       (other-primary-key 'id)
       (other-foreign-key (to-foreign-key other-class))
       (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/ ~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/ ~
"
                            table table foreign-key class primary-key
                            other-class other-class other-primary-key table other-foreign-key )))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'reldat-hbtm
                    :class ',class
                    :slot ',slot
                    :table ',table
                    :other-class ',other-class
                    :primary-key ',primary-key
                    :foreign-key ',foreign-key
                    :other-primary-key ',other-primary-key
                    :other-foreign-key ',other-foreign-key
                    :join-clause ,join-clause)
     (defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
       (setf (slot-value instance slot-name)
             (if (persistedp instance)
                 (fetch (query ',other-class
                          (join ,(format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                         table table other-foreign-key other-class other-primary-key))
                          (where ',foreign-key (slot-value instance ',primary-key)))))))))

(defmethod reldat-preload ((reldat reldat-has-many) records class slot)
  (let* ((primary-key (slot-value reldat 'primary-key))
         (foreign-key (slot-value reldat 'foreign-key))
         (other-class (slot-value reldat 'other-class))
         (ids (delete-duplicates (loop for record in records
                                       collect (slot-value record primary-key))
                                 :test #'equal))
         (children (fetch (query other-class
                            (where foreign-key ids)
                            (when (and (slot-exists-p reldat 'order)
                                       (slot-boundp reldat 'order)
                                       (slot-value reldat 'order))
                              (order (slot-value reldat 'order)))))))
    (loop for record in records
          do (setf (slot-value record slot)
                   (loop for child in children
                         if (equal (slot-value child foreign-key)
                                   (slot-value record primary-key))
                           collect child)))
    (values children other-class)))

(defmethod reldat-preload ((reldat reldat-has-one) records class slot)
  (let* ((primary-key (slot-value reldat 'primary-key))
         (foreign-key (slot-value reldat 'foreign-key))
         (other-class (slot-value reldat 'other-class))
         (ids (delete-duplicates (loop for record in records
                                       collect (slot-value record primary-key))
                                 :test #'equal))
         (children (fetch (query other-class
                            (where foreign-key ids)
                            (when (and (slot-exists-p reldat 'order)
                                       (slot-boundp reldat 'order)
                                       (slot-value reldat 'order))
                              (order (slot-value reldat 'order)))))))
    (loop for record in records
          do (setf (slot-value record slot)
                   (loop for child in children
                           thereis (and (equal (slot-value child foreign-key)
                                               (slot-value record primary-key))
                                        child))))
    (values children other-class)))

(defmethod reldat-preload ((reldat reldat-hbtm) records class slot)
  (let* ((primary-key (slot-value reldat 'primary-key))
         (foreign-key (slot-value reldat 'foreign-key))
         (other-class (slot-value reldat 'other-class))
         (table (slot-value reldat 'table))
         (ids (delete-duplicates (loop for record in records
                                       collect (slot-value record primary-key))
                                 :test #'equal))
         (recordset (execute (sql (query other-class
                                    (select (format nil "~/dbq::tbl/.~/dbq::col/, ~/dbq::tbl/.*"
                                                    table foreign-key class))
                                    (join (reverse-join-clause reldat))
                                    (where foreign-key ids))))))


    (let ((id_children (loop for ((_ . id) . row) in (nreverse recordset)
                             collect (cons id (car (store other-class (list row)))))))
      (loop for record in records
            do (setf (slot-value record slot)
                     (loop for (id . child) in id_children
                           if (equal id (slot-value record primary-key))
                             collect child)))
      (values (loop for (id . child) in id_children
                    collect child)
              other-class))))

(defmethod reldat-preload ((reldat reldat-belongs-to) records class slot)
  (let* ((primary-key (slot-value reldat 'primary-key))
         (foreign-key (slot-value reldat 'foreign-key))
         (other-class (slot-value reldat 'other-class))
         (ids (delete-duplicates (loop for record in records
                                       collect (slot-value record foreign-key))
                                 :test #'equal))
         (children (fetch (query other-class
                            (where primary-key ids)))))
    (loop for record in records
          do (setf (slot-value record slot) nil))
    (loop for record in records
          for child = (find-if (lambda (child)
                                 (equal (slot-value child primary-key)
                                        (slot-value record foreign-key)))
                               children)
          do (setf (slot-value record slot) child))
    (values children other-class)))
