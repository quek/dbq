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
