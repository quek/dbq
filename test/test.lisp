(in-package :dbq.test)

(let ((db-name "dbq_test"))
  (asdf:run-shell-command
   "echo 'CREATE DATABASE IF NOT EXISTS `~a` DEFAULT CHARACTER SET utf8mb4' | mysql -uroot"
   db-name)
  (establish-connection :user "root"
                        :database db-name))

(progn
  (execute "drop table if exists `entries`")
  (execute "create table entries (
  `id` int(11) not null auto_increment,
  `title` varchar(100) not null,
  `content` text not null,
  `user_id` int,
  `created_at` datetime not null,
  `updated_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  (execute "drop table if exists `comments`")
  (execute "create table comments (
  `id` int(11) not null auto_increment,
  `entry_id` int(11) not null,
  `content` text not null,
  `created_at` datetime not null,
  `updated_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  (execute "drop table if exists `categories`")
  (execute "create table categories (
  `id` int(11) not null auto_increment,
  `name` varchar(255) not null,
  `created_at` datetime not null,
  `updated_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  (execute "drop table if exists `category_entries`")
  (execute "create table `category_entries` (
  `category_id` int not null,
  `entry_id` int not null,
  primary key (`category_id`, `entry_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  (execute "drop table if exists `users`")
  (execute "create table `users` (
  `id` int(11) not null auto_increment,
  `name` varchar(100) not null,
  `created_at` datetime not null,
  `updated_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  )

(defclass entry (dao-mixin)
  ((title :initarg :title :accessor title-of)
   (content :initarg :content :accessor content-of)
   (user-id :initarg :user-id :accessor user-id-of)
   (comments :initarg :comments :accessor comments-of)
   (categories :initarg :categories :accessor categories-of)))

(defclass comment (dao-mixin)
  ((entry-id :initarg :entry-id :accessor entry-id-of)
   (content :initarg :content :accessor content-of)))

(defclass category (dao-mixin)
  ((name :initarg :name :accessor name-sf)))

(defclass user (dao-mixin)
  ((name :initarg :name :accessor name-of)))

(def-has-many :class entry :slot comments :other-class comment)
(def-hbtm :class entry :slot categories :other-class category :table "category_entries")
(def-belongs-to :class entry :other-class user)

(deftest save-and-find-by-id ()
  (let ((entry (make-instance 'entry :title "題名" :content "本文")))
    (is (save entry))
    (let ((id (id-of entry)))
      (is (numberp id))
      (let ((entry (fetch-one (query 'entry (where :id id)) :class 'entry)))
        (describe entry)
        (is (equal id (id-of entry)))
        (is (equal "題名" (title-of entry)))
        (is (equal "本文" (content-of entry)))))))

(deftest transaction-test ()
  (let ((entry (make-instance 'entry :title "a" :content "b")))
    (save entry)
    (with-transaction
      (setf (title-of entry) "AA")
      (save entry)
      (rollback))
    (is (string= "a" (title-of (find-by 'entry :id (id-of entry)))))
    (with-transaction
      (setf (title-of entry) "AA")
      (save entry))
    (is (string= "AA" (title-of (find-by 'entry :id (id-of entry)))))))

(deftest hbtm-test ()
  (let ((category1 (make-instance 'category :name "プログラミング"))
        (category2 (make-instance 'category :name "読書"))
        (entry (make-instance 'entry :title "題名" :content "本文")))
    (save category1)
    (save category2)
    (save entry)
    (execute (format nil "insert into category_entries values(~d, ~d)"
                     (id-of category1) (id-of entry)))
    (execute (format nil "insert into category_entries values(~d, ~d)"
                     (id-of category2) (id-of entry)))
    (is (= (id-of entry)
           (id-of (fetch-one (query 'entry (join 'categories)
                               (where :categories.id (id-of category1)))))))
    (is (equal (list (id-of category1) (id-of category2))
               (mapcar #'id-of (categories-of entry))))))

(deftest hbtm-insert-test ()
  (let* ((category1 (make-instance 'category :name "プログラミング"))
         (category2 (make-instance 'category :name "読書"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :categories (list category1 category2))))
    (save entry)
    (let ((entry (find-by 'entry :id (id-of entry))))
      (is (equal (list (id-of category1) (id-of category2))
                 (mapcar #'id-of (categories-of entry)))))
    (is (id= entry
             (fetch-one (query 'entry
                          (join 'categories)
                          (where :categories.id (id-of category1))))))))

(deftest has-many-insert-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (save entry)
    (let ((entry (find-by 'entry :id (id-of entry))))
      (is (equal (list (id-of comment1) (id-of comment2))
                 (mapcar #'id-of (comments-of entry)))))
    (is (id= entry
             (fetch-one (query 'entry
                          (join 'comments)
                          (where :comments.id (id-of comment1))))))))

(run-package-tests :interactive t)
