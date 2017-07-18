(in-package :dbq.test)

(defclass entry (dao-mixin)
  ((title :initarg :title :accessor title-of)
   (content :initarg :content :accessor content-of)
   (user-id :initarg :user-id :accessor user-id-of)
   (user :initarg :user :accessor user-of)
   (comments :initarg :comments :accessor comments-of)
   (categories :initarg :categories :accessor categories-of)))

(defclass comment (dao-mixin)
  ((entry-id :initarg :entry-id :accessor entry-id-of)
   (content :initarg :content :accessor content-of)))

(defclass category (dao-mixin)
  ((name :initarg :name :accessor name-sf)))

(defclass user (dao-mixin)
  ((name :initarg :name :accessor name-of)))
