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
   (comments)))

(fiasco:deftest save-and-find-by-id ()
  (let ((entry (make-instance 'entry :title "題名" :content "本文")))
    (fiasco:is (save entry))
    (let ((id (id-of entry)))
      (fiasco:is (numberp id))
      (let ((entry (fetch-one (query 'entries (where :id id)) :class 'entry)))
        (describe entry)
        (fiasco:is (equal id (id-of entry)))
        (fiasco:is (equal "題名" (title-of entry)))
        (fiasco:is (equal "本文" (content-of entry)))))))

(fiasco:run-package-tests :interactive t)
