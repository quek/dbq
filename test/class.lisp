(in-package :dbq.test)

(defclass entry (dbq:dao-mixin)
  ((title :initarg :title :accessor .title)
   (content :initarg :content :accessor .content)
   (user-id :initarg :user-id :accessor .user-id)
   (user :initarg :user :accessor .user)
   (comments :initarg :comments :accessor .comments)
   (categories :initarg :categories :accessor .categories)))

(defclass comment (dbq:dao-mixin)
  ((entry-id :initarg :entry-id :accessor .entry-id)
   (content :initarg :content :accessor .content)
   (entry :initarg :entry :accessor .entry)))

(defclass category (dbq:dao-mixin)
  ((name :initarg :name :accessor name-sf)))

(defclass user (dbq:dao-mixin)
  ((name :initarg :name :accessor .name)
   (entries :initarg :entries :accessor .entries)
   (comments :initarg :comments :accessor .comments)))

(defclass community (dbq:dao-mixin)
  ((name :initarg :name :accessor .name)
   (community-members :initarg :community-members :accessor .community-members)
   (users :initarg :users :accessor .users)
   (entries :initarg :entries :accessor .entries)
   (comments :initarg :comments :accessor .comments)))

(defclass community-member (dbq:dao-mixin)
  ((role :initarg :role :accessor .role)
   (user-id :initarg :user-id :accessor .user-id)
   (user :initarg :user :accessor .user)
   (community-id :initarg :community-id :accessor .community-id)
   (community :initarg :community :accessor .community)))
