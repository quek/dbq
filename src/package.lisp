;;;; package.lisp

(defpackage :dbq
  (:use :cl :anaphora)
  (:shadow #:count #:delete #:<= #:>= #:< #:>)
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

   #:def-migration
   #:migrate
   #:migrate-down
   ))
