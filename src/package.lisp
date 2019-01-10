;;;; package.lisp

(defpackage :dbq
  (:use :cl :anaphora)
  (:shadow #:count #:delete #:time #:<= #:>= #:< #:>)
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
   #:.id
   #:id=
   #:created-at
   #:.created-at
   #:updated-at
   #:.updated-at
   #:has-many
   #:save
   #:create
   #:update
   #:delete
   #:find-by

   #:define-hbtm
   #:define-has-many
   #:define-has-one
   #:define-belongs-to

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
   #:load-relations
   #:sql

   #:<=
   #:>=
   #:<
   #:>

   #:to-foreign-key

   #:location
   #:location-lat
   #:location-lng
   #:make-location
   #:enable-read-geography-point

   #:time
   #:.hour
   #:.minute
   #:.second
   #:.microsecond
   #:time-from-string

   #:def-migration
   #:migrate
   #:migrate-down
   ))
