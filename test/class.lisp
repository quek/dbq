(in-package :dbq.test)

(defclass entry (dbq:dao-mixin)
  ((title :initarg :title :accessor title-of)
   (content :initarg :content :accessor content-of)
   (user-id :initarg :user-id :accessor user-id-of)
   (user :initarg :user :accessor user-of)
   (comments :initarg :comments :accessor comments-of)
   (categories :initarg :categories :accessor categories-of)))

(defclass comment (dbq:dao-mixin)
  ((entry-id :initarg :entry-id :accessor entry-id-of)
   (content :initarg :content :accessor content-of)
   (entry :initarg :entry :accessor entry-of)))

(defclass category (dbq:dao-mixin)
  ((name :initarg :name :accessor name-sf)))

(defclass user (dbq:dao-mixin)
  ((name :initarg :name :accessor name-of)
   (entries :initarg :entries :accessor entries-of)
   (comments :initarg :comments :accessor comments-of)))

(defclass community (dbq:dao-mixin)
  ((name :initarg :name :accessor name-of)
   (community-members :initarg :community-members :accessor community-members-of)
   (users :initarg :users :accessor users-of)))

(defclass community-member (dbq:dao-mixin)
  ((role :initarg :role :accessor role-of)
   (user-id :initarg :user-id :accessor user-id-of)
   (user :initarg :user :accessor user-of)
   (community-id :initarg :community-id :accessor community-id-of)
   (community :initarg :community :accessor community-of)))
