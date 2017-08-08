(in-package :dbq)


(setq json::+json-lisp-symbol-tokens+
      '(("true" . t)
        ("null" . :null)
        ("false" . nil)))

;; TODO これきれいじゃないから、なんか違う
(defun json (thing &rest slots)
  (json:with-explicit-encoder
    (json:encode-json-to-string
     (if (and (consp thing)
              (keywordp (car thing)))
         (cons :alist
               (loop for (key . slots) in slots
                     collect (cons key (apply #'%json (getf thing key) slots))))
         (apply #'%json thing slots)))))

(defmethod %json ((instance standard-object) &rest slots)
  (cons :alist
        (loop for slot in slots
         if (atom slot)
           collect (cons slot (json-value instance slot))
         else
           collect (let* ((slots (cdr slot))
                          (slot (car slot))
                          (value (json-value instance slot)))
                     (cons slot (apply #'%json value slots))))))

(defmethod %json ((instance list) &rest slots)
  (cons :list (mapcar (lambda (x) (apply #'%json x slots)) instance)))

(defmethod %json ((instance (eql :null)) &rest slots)
  (declare (ignore slots))
  #())

(defun json-value (instance slot)
  (if (slot-exists-p instance slot)
      (slot-value instance slot)
      (funcall slot instance)))

(defmethod json:encode-json ((location location) &optional (stream json:*json-output*))
  (format stream "{\"lat\":~f,\"lng\":~f}" (location-lat location) (location-lng location)))

(defmethod (setf json) ((json string) instance &rest slots)
  (let* ((json:*identifier-name-to-key* #'identity)
         (json (json:decode-json-from-source json)))
    (apply #'(setf json) json instance slots)))

(defmethod (setf json) (json instance &rest slots)
  (loop for slot in slots
        if (atom slot)
          do (awhen (assoc slot json :test #'string-equal)
               (setf (slot-value instance slot)
                     (cdr it)))
        else
          do (let* ((slots (cdr slot))
                    (slot (car slot))
                    (json (cdr (assoc slot json :test #'string-equal))))
               (cond ((has-many-slot-p instance slot)
                      (has-many-from-json instance slot slots json))
                     ((hbtm-slot-p instance slot)
                      (hbtm-from-json instance slot slots json)))))
  instance)

(defun has-many-from-json (instance slot slots json)
  (let ((class (has-many-other-class (class-name (class-of instance))
                                     slot)))
    (setf (slot-value instance slot)
          (loop for j in json
                collect (apply #'(setf json) j (make-instance class) slots)))))

;; TODO 上の has-many-from-json と共通化だな
(defun hbtm-from-json (instance slot slots json)
  (let ((class (hbtm-other-class (class-name (class-of instance))
                                 slot)))
    (setf (slot-value instance slot)
          (loop for j in json
                collect (apply #'(setf json) j (make-instance class) slots)))))
