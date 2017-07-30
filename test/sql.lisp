(in-package :dbq.test)

;; sudo -u postgres psql
;; create table dbq_test
;; alter user ancient with encrypted password 'neko';
(let ((db-name "dbq_test"))
  #+nil
  (asdf:run-shell-command
   "echo 'CREATE DATABASE IF NOT EXISTS `~a` DEFAULT CHARACTER SET utf8mb4' | mysql -uroot"
   db-name)
  (establish-connection :user "ancient"
                        :password "neko"
                        :database db-name))

(progn
  (execute "drop table if exists entries")
  (execute "create table entries (
  id serial primary key,
  title varchar(100) not null,
  content text not null,
  user_id int,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (execute "drop table if exists comments")
  (execute "create table comments (
  id serial primary key,
  entry_id integer not null,
  content text not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (execute "drop table if exists categories")
  (execute "create table categories (
  id serial primary key,
  name varchar(255) not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (execute "drop table if exists category_entries")
  (execute "create table category_entries (
  category_id integer not null,
  entry_id integer not null)")
  (execute "drop table if exists users")
  (execute "create table users (
  id serial primary key,
  name varchar(100) not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  )
