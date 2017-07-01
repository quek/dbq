(in-package :dbq.test)

(5am:def-suite dbq)
(5am:in-suite dbq)

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
   (updated-at :initform (local-time:now) :accessor updated-at)))

(5am:test save-and-find-by-id
  (let ((entry (make-instance 'entry :title "題名" :content "本文")))
    (5am:is (save entry))
    (let ((id (id-of entry)))
      (5am:is (numberp id))
      (let ((x (find-by 'entry :id id)))
        (5am:is (equal id (id-of x)))
        (5am:is (equal "題名" (title-of entry)))
        (5am:is (equal "本文" (content-of entry)))))))


(5am:debug!)
