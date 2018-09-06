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

(defmacro define-has-many
    (class slot
     &key (other-class (sym (singularize slot)))
       through
       (primary-key 'id)
       (foreign-key (if through
                        (to-foreign-key (slot-value (reldat class through) 'other-class))
                        (to-foreign-key class)))
       (join-clause (if through
                        (let ((reldat (reldat class through)))
                          (format nil "~
~a ~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                  (slot-value reldat 'join-clause)
                                  other-class other-class foreign-key
                                  (slot-value reldat 'other-class) primary-key))
                        (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                other-class other-class foreign-key class primary-key)))
       order)
  (make-instance 'reldat-has-many
                 :class class
                 :slot slot
                 :other-class other-class
                 :foreign-key foreign-key
                 :primary-key primary-key
                 :through through
                 :join-clause join-clause
                 :order order)
  `(defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
     (if *query-builder*
         (setf *query-builder*
               ,(if through
                    (let ((reldat (reldat class through)))
                      `(query ',other-class
                         (join ,(format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                        (slot-value reldat 'other-class)
                                        other-class foreign-key
                                        (slot-value reldat 'other-class) primary-key))
                         (where ',(slot-value reldat 'foreign-key) (id-of instance))))
                    `(query ',other-class
                       (where ',foreign-key (id-of instance)))))
         (setf (slot-value instance slot-name)
               (if (persistedp instance)
                   (fetch (query ',other-class
                            (where ',foreign-key (id-of instance))
                            ,@(when order `((order ,order))))))))))

(defmacro define-has-one
    (class slot
     &key (other-class (sym (singularize slot)))
       (primary-key 'id)
       (foreign-key (sym class "-id"))
       through
       (join-clause (if through
                        (let ((reldat (reldat class through)))
                          ;; TODO
                         (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                 other-class other-class foreign-key class primary-key))
                        (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                other-class other-class foreign-key class primary-key)))
       order)
  (make-instance 'reldat-has-many
                 :class class
                 :slot slot
                 :other-class other-class
                 :foreign-key foreign-key
                 :through through
                 :join-clause join-clause)
  `(defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
     (if *query-builder*
         (setf *query-builder*
               ,(if through
                    (let ((reldat (reldat class through)))
                      `(query ',other-class
                         (join ,join-clause)
                         (join ,(slot-value reldat 'join-clause))
                         (where ',(slot-value reldat 'foreign-key) (id-of instance))))
                    `(query ',other-class
                       (where ',foreign-key (id-of instance)))))
         (setf (slot-value instance slot-name)
               (if (persistedp instance)
                   (fetch-one (query ',other-class
                            (where ',foreign-key (id-of instance))
                                ,@(when order `((order ,order)))
                                (limit 1))))))))

(defmacro define-belongs-to
    (class slot
     &key (other-class slot)
       (primary-key 'id)
       (foreign-key (sym slot "-id"))
       (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                            other-class other-class primary-key class foreign-key ))
       (writer `(setf ,(sym other-class "-OF"))))
  (make-instance 'reldat-belongs-to
                 :class class
                 :slot slot
                 :other-class other-class
                 :foreign-key foreign-key
                 :join-clause join-clause)
  `(progn
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
       (foreign-key (sym class "-id"))
       (other-primary-key 'id)
       (other-foreign-key (sym other-class "-id"))
       (join-clause (format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/ ~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/ ~
"
                            table table foreign-key class primary-key
                            other-class other-class other-primary-key table other-foreign-key )))
  (make-instance 'reldat-hbtm
                 :class class
                 :slot slot
                 :table table
                 :other-class other-class
                 :primary-key primary-key
                 :foreign-key foreign-key
                 :other-primary-key other-primary-key
                 :other-foreign-key other-foreign-key
                 :join-clause join-clause)
  `(defmethod slot-unbound (class (instance ,class) (slot-name (eql ',slot)))
     (setf (slot-value instance slot-name)
           (if (persistedp instance)
               (fetch (query ',other-class
                        (join ,(format nil "~
inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/=~/dbq::tbl/.~/dbq::col/"
                                       table table other-foreign-key other-class other-primary-key))
                        (where ',foreign-key (slot-value instance ',primary-key))))))))
