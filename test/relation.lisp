(in-package :dbq.test)

(dbq::define-has-many entry comments)

(dbq::define-hbtm entry categories "category_entries")

(dbq::define-belongs-to entry user)

(dbq::define-has-many user entries)

(dbq::define-has-many user comments :through entries)

(dbq::define-belongs-to comment entry)

(dbq::define-has-many community community-members)

(dbq::define-has-many community users :through community-members)
