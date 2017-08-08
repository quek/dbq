;;;; package.lisp

(defpackage :dbq
  (:use :cl :anaphora)
  (:shadow #:count #:delete)
  (:export
   ;; connection
   #:establish-connection
   #:disconnect
   #:with-connection
   #:execute
   #:with-transaction
   #:rollback

   ;;
   #:id-mixin
   #:created-at-mixin
   #:updated-at-mixin
   #:dao-mixin
   #:id
   #:id-of
   #:id=
   #:created-at
   #:updated-at
   #:has-many
   #:save
   #:find-by

   #:def-hbtm
   #:def-has-many
   #:def-belongs-to

   ;;
   #:fetch-one
   #:fetch
   #:count
   #:query
   #:select
   #:from
   #:join
   #:where
   #:order
   #:group
   #:offset
   #:limit
   #:page
   #:per-page
   #:preload
   #:sql

   #:def-migration
   #:migrate
   #:migrate-up
   #:migrate-down

   #:json
   ))
