(in-package :dbq)


(defun json (instance &rest slots)
  (json:with-explicit-encoder
    (json:encode-json-to-string (apply #'%json instance slots))))

(defmethod %json (instance &rest slots)
  (cons :alist
        (loop for slot in slots
         if (atom slot)
           collect (cons slot (slot-value instance slot))
         else
           collect (let* ((slots (cdr slot))
                          (slot (car slot))
                          (value (slot-value instance slot)))
                     (cond ((has-many-slot-p instance slot)
                            (cons slot (apply #'%json value slots))))))))

(defmethod %json ((instance cons) &rest slots)
  (cons :list (mapcar (lambda (x) (apply #'%json x slots)) instance)))

(defmethod (setf json) ((json string) instance &rest slots)
  (let* ((json:*identifier-name-to-key* #'identity)
         (json (json:decode-json-from-source json)))
    (apply #'(setf json) json instance slots)))

(defmethod (setf json) (json instance &rest slots)
  (loop for slot in slots
        if (atom slot)
          do (setf (slot-value instance slot)
                   (cdr (assoc slot json :test #'string-equal)))
        else
          do (let* ((slots (cdr slot))
                    (slot (car slot))
                    (json (cdr (assoc slot json :test #'string-equal))))
               (cond ((has-many-slot-p instance slot)
                      (has-many-from-json instance slot slots json)))))
  instance)

(defun has-many-from-json (instance slot slots json)
  (let ((class (has-many-other-class (class-name (class-of instance))
                                          slot)))
    (setf (slot-value instance slot)
          (loop for j in json
                collect (apply #'(setf json) j (make-instance class) slots)))))
