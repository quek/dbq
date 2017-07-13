(in-package :dbq)

(defmethod json (instance &rest slots)
  (json:encode-json-alist-to-string
   (loop for slot in slots
         if (atom slot)
           collect (cons slot (slot-value instance slot))
         else
           collect ;; TODO いろいろやる
           (cons slot (slot-value instance (car slot))))))

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
