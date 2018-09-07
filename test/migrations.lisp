(in-package :dbq.test)

;; sudo -u postgres psql -c "CREATE ROLE ancient LOGIN CREATEDB PASSWORD 'neko';"
;; sudo -u postgres psql -c "CREATE DATABASE dbq_test OWNER ancient;"
(let ((db-name "dbq_test"))
  #+nil
  (asdf:run-shell-command
   "echo 'CREATE DATABASE IF NOT EXISTS `~a` DEFAULT CHARACTER SET utf8mb4' | mysql -uroot"
   db-name)
  (dbq:establish-connection :user "ancient"
                        :password "neko"
                        :database db-name))

(dbq:def-migration 20170808085920-create-entries
    (dbq:execute "create table entries (
  id serial primary key,
  title varchar(100) not null,
  content text not null,
  user_id int,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (dbq:execute "drop table if exists entries"))

(dbq:def-migration 20170808090042-create-comments
    (dbq:execute "create table comments (
  id serial primary key,
  entry_id integer not null,
  content text not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (dbq:execute "drop table if exists comments"))

(dbq:def-migration 20170808090120-create-categories
    (dbq:execute "create table categories (
  id serial primary key,
  name varchar(255) not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (dbq:execute "drop table if exists categories"))

(dbq:def-migration 20170808090203-create-category-entries
    (dbq:execute "create table category_entries (
  category_id integer not null,
  entry_id integer not null)")
  (dbq:execute "drop table if exists category_entries"))

(dbq:def-migration 20170808090252-create-users
    (dbq:execute "create table users (
  id serial primary key,
  name varchar(100) not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (dbq:execute "drop table if exists users"))

(dbq:def-migration 20180907162850-create-communities
    (dbq:execute "create table communities (
  id serial primary key,
  name varchar(100) not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (dbq:execute "drop table if exists communities"))

(dbq:def-migration 20180907163015-create-community-members
    (dbq:execute "create table community_members (
  id serial primary key,
  role varchar(100) not null,
  user_id integer not null,
  community_id integer not null,
  created_at timestamp not null,
  updated_at timestamp not null)")
  (dbq:execute "drop table if exists community_members"))

;; (dbq:migrate-down :dbq.test '20180907163015-create-members)

(dbq:migrate :dbq.test)
