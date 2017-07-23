;;;; package.lisp

(fiasco:define-test-package :dbq.test
  (:use :cl :anaphora :dbq :fiasco)
  (:shadowing-import-from :dbq #:count #:delete))
