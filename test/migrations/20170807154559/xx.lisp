(in-package :dbq.test)

(dbq::def-migration 20170807154559-create-tables
    (progn
      (execute "create table queque (id integer)"))
  (progn
    (execute "drop table queque")))

(dbq::def-migration 20170807162800-create-tables
    (progn
      (execute "create table nyaaa (id integer)"))
  (progn
    (execute "drop table nyaaa")))

(dbq::migrate :dbq.test)

(dbq::migrate-down :dbq.test '20170807162800-create-tables)
