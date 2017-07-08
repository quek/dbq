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
  )

(defclass entry (dao-mixin)
  ((id :accessor id-of)
   (title :initarg :title :accessor title-of)
   (content :initarg :content :accessor content-of)
   (created-at :initform (local-time:now) :accessor created-at)
   (updated-at :initform (local-time:now) :accessor updated-at)
   (comments :accessor comments-of)
   (categories :accessor categories-of)))

(def-hbtm :class entry :slot categories :join-clause "
inner join category_entries on category_entries.entry_id=entries.id
inner join categories on categories.id = category_entries.category_id
")

(defclass category (dao-mixin)
  ((name :initarg :name :accessor name-sf)))

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
  (let ((category (make-instance 'category :name "プログラミング"))
        (entry (make-instance 'entry :title "題名" :content "本文")))
    (save category)
    (save entry)
    (execute (format nil "insert into category_entries values(~d, ~d)"
                     (id-of category) (id-of entry)))
    (is (= (id-of entry)
           (id-of (fetch-one (query 'entry (join 'categories)
                               (where :categories.id (id-of category)))))))))

(run-package-tests :interactive t)
