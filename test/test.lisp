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
  (execute "drop table if exists `tags`")
  (execute "create table `tags` (
  `id` int(11) not null auto_increment,
  `name` varchar(100) not null,
  `created_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  (execute "drop table if exists `taggings`")
  (execute "create table `taggings` (
  `id` int(11) not null auto_increment,
  `tag_id` int(11) not null,
  `taggable_id` int(11) not null,
  `taggable_type` varchar(255) not null,
  `created_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4")
  )

(defclass entry ()
  ((dbq:id :accessor id-of)
   (title :initarg :title :accessor title-of)
   (content :initarg :content :accessor content-of)
   (created-at :initform (local-time:now) :accessor created-at)
   (updated-at :initform (local-time:now) :accessor updated-at)
   (comments)
   (dbq:has-many :initform '(comments) :allocation :class)))

;;(sb-mop:finalize-inheritance (find-class 'entry))

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

(run-package-tests :interactive t)
