(in-package :dbq)

(defvar *migrations* (make-hash-table))

(defun migrate (package)
  (ensure-migration-table)
  (let* ((migrations (or (gethash (find-package package) *migrations*)
                         (return-from migrate nil)))
         (keys (sort (loop for key being the hash-key of migrations
                           collect key)
                     #'string< :key #'symbol-name)))
    (loop for key in keys
          do (migrate-call-up key (car (gethash key migrations))))))

(defun migrate-down (package version)
  (let* ((migrations (gethash (find-package package) *migrations*))
         (function (cdr (or (gethash version migrations)
                            (error "No such migration ~a::~a!" package version)))))
    (with-transaction
      (when (execute (format nil "select * from migrations where version = ~/dbq::val/"
                             (symbol-name version)))
        (execute (format nil "delete from migrations where version = ~/dbq::val/"
                         (symbol-name version)))
        (funcall function)))))

(defun ensure-migration-table ()
  (execute "create table if not exists migrations (
version varchar(255) primary key
)"))

(defun migrate-call-up (version up)
  (with-transaction
    (unless (execute (format nil "select * from migrations where version = ~/dbq::val/"
                             (symbol-name version)))
      (execute (format nil "insert into migrations values (~/dbq::val/)" (symbol-name version)))
      (funcall up))))

(defmacro define-migration (name option up down)
  (declare (ignore option))
  (let ((package (gensym)))
    `(let ((,package (symbol-package ',name)))
       (unless (gethash ,package *migrations*)
         (setf (gethash ,package *migrations*) (make-hash-table)))
       (setf (gethash ',name (gethash ,package *migrations*))
             (cons (lambda () ,up)
                   (lambda () ,down))))))
