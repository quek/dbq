拡張シーケンス使う？
(query (relation (voices-of temple))
  (page 1)
  (per-page 10))



(defclass table ()
  ((attributes)
   (associations)
   (table-name :allocation :class)))

(defclass attribute ()
  ((value)
   (type)
   (name)
   (column-name)
   (old-value)
   (before-type-cast-value)))

(defclass association ()
  ((name)
   (parent-table)
   (child-table)
   (join)
   (scope)))


(defclass scope ()
  ())

(defclass where ()
  ())


(where :status :> 3
        :owner :not ids
        :valide t
        :foo :null
        :bar :not-null)

(order :id :desc :created_at :desc)


(defclass user (base)
  ()
  (:has-many :blogs :dependent :destroy)
  (:has-many :entries :through :blogs
              :scope (lambda () (order :created_at :desc)))
  (:has-many :comments :through :entries))

(defclass blog (base)
  ()
  (:belogns-to :user)
  (:has-many :entries :dependent :destroy)
  (:has-many :recent-entries
              :scope (lambda ()
                       (where :created_at :>= (date- (now) 7))
                       (order :created_at :desc)
                       (limit 10))))

(defclass entry (base)
  ()
  (:belogns-to :blog)
  (:has-many :comments :dependent :destroy))

(defclass comment (base)
  ()
  (:belongs-to :entry))


(user
 (comments)
 (where :content :lick "%foo%"))


(defscope published ((- facilities))
  (where :publish true))
;;->
(progn
  (defun publised ()
    (%published *context*))
  (defmethod published (- facilities)
    (where :publish true)))

(events
 (published)
 (joins :coupon :premium-ad)
 (merge facilities #'publish))



(query 'event
       (where :published true)
       (join :coupon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(l :info.read-eval-print.double-quote)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defstruct qb
  (select "*")
  from
  where
  order
  having
  group
  limit
  offset)

(defvar *qb* nil)

(defun select (select)
  (setf *qb* (copy-qb *qb*))
  (setf (qb-select *qb*) select)
  *qb*)

(defun from (from)
  (setf *qb* (copy-qb *qb*))
  (setf (qb-from *qb*) from)
  *qb*)

(defun where (&rest where)
  (setf *qb* (copy-qb *qb*))
  (setf (qb-where *qb*) (append (qb-where *qb*) where))
  *qb*)

(defgeneric escape-sql-value (value))
(defmethod escape-sql-value ((value (eql t)))
  "true")
(defmethod escape-sql-value ((value string))
  (concatenate 'string "'" (ppcre:regex-replace-all "'" value "''") "'"))

(defun sql (qb)
  (with-output-to-string (*standard-output*)
    (format t
            "select ~a from ~a"
            (qb-select qb)
            (qb-from qb))
    (let ((where (qb-where qb)))
      (when where
          (write-string " where ")
          (loop with and = ""
                while where
                do (write-string and)
                   (setf and " and ")
                   (let ((x (pop where)))
                     (typecase x
                       (symbol (format t "~a=~a" x
                                       (escape-sql-value (pop where)))))))))))

(defgeneric to-qb (x))
(defmethod to-qb ((x qb))
  x)
(defmethod to-qb ((x symbol))
  (make-qb :from x))

(defmacro query (qb &body body)
  `(let ((*qb* (to-qb ,qb)))
     ,@body))

(sql (let ((*qb* (make-qb)))
       (from  "users")))
;;⇒ "select * from users where 1=1"

(sql (query (make-qb)
       (from  "users")
       (when t
         (where :activated t))
       (where :name "neko")))
;;⇒ "select * from users where ACTIVATED=true and NAME='neko'"

(sql (query 'users
       (when t
         (where :activated t))
       (where :name "neko")))
;;⇒ "select * from USERS where ACTIVATED=true and NAME='neko'"



(defclass user ()
  ((id)
   (name :initarg :name
            :accessor user-name)
   (email :initarg :email
          :accessor user-email)
   (foo :initarg :foo
        :accessor user-foo)))

(defun set-value (object slot-name value)
  (let ((slot (find slot-name (sb-mop:class-slots (class-of object))
                    :test #'string-equal
                    :key #'sb-mop:slot-definition-name)))
    (if slot
        (setf (slot-value object (sb-mop:slot-definition-name slot)) value)
        (format t "no slot for ~a ~a" slot-name value))))

(defun store (class rows columns)
  (loop for row in rows
        collect (let ((object (make-instance class)))
                  (loop for value in row
                        for (column-name column-type) in columns
                        do (set-value object column-name value))
                  object)))


(let ((c (cl-mysql:connect :user "root" :database "mito" )))
  (unwind-protect
       (progn
         (destructuring-bind ((rows columns)) (cl-mysql:query "select * from user")
           (store 'user rows columns)))
    (cl-mysql:disconnect c)))



(class-of (make-instance 'user))
;;⇒ #<USER {1003962133}>

(let* ((slots (sb-mop:class-slots (find-class 'user)))
       (slot (car slots)))
  (describe slot)
  (sb-mop:slot-definition-writers (car (sb-mop:class-direct-slots (find-class 'user))))
;;⇒ NIL

                                        ;(describe (sb-mop:slot-definition-writers slot))
  
  (sb-mop:slot-definition-name slot))

