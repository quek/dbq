(in-package :dbq)

(defclass operator ()
  ((operand :initarg :operand :accessor operand-of)))

(defgeneric operator-of (operator))

(defclass <= (operator)
  ())

(defmethod operator-of ((op <=))
  "<=")

(defun <= (operand)
  (make-instance `<= :operand operand))

(defclass >= (operator)
  ())

(defmethod operator-of ((op >=))
  ">=")

(defun >= (operand)
  (make-instance `>= :operand operand))
