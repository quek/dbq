(in-package :dbq)

(defun update-hbtm (record)
  (loop with class = (class-name (class-of record))
        for slot in (hbtm-slots class)
        for reldat = (reldat class slot)
        if (slot-boundp record slot)
          do (execute (format nil "delete from ~/dbq::tbl/ where ~/dbq::col/=~/dbq::val/"
                              (slot-value reldat 'table)
                              (slot-value reldat 'foreign-key)
                              (slot-value record (slot-value reldat 'primary-key))))
             (loop for x in (slot-value record slot)
                   unless (persistedp x)
                     do (save x)
                   do (execute (format nil "~
insert into ~/dbq::tbl/ (~/dbq::col/, ~/dbq::col/) values(~/dbq::val/, ~/dbq::val/)"
                                       (slot-value reldat 'table)
                                       (slot-value reldat 'foreign-key)
                                       (slot-value reldat 'other-foreign-key)
                                       (slot-value record (slot-value reldat 'primary-key))
                                       (slot-value x (slot-value reldat 'other-primary-key)))))))

(defun preload-hbtm (records query)
  (loop with class = (query-builder-from query)
        with ids = (delete-duplicates (mapcar #'id-of records))
        for slot in (intersection (query-builder-preload query)
                                  (hbtm-slots class))
        for other-class = (hbtm-other-class class slot)
        for table = (hbtm-table class slot)
        for sql = (format nil "select ~/dbq::tbl/.~/dbq::col/, ~/dbq::tbl/.* ~
from ~/dbq::tbl/ inner join ~/dbq::tbl/ on ~/dbq::tbl/.~/dbq::col/ = ~/dbq::tbl/.id ~
where ~/dbq::tbl/.~/dbq::col/ in ~/dbq::val/"
                          table (sym class "-ID") other-class
                          other-class table table (sym other-class "-ID") other-class
                          table (sym class "-ID") ids)
        for results = (execute sql)
        do (loop for record in records
                 do (setf (slot-value record slot) nil))
           (loop for ((_ . id) . row) in (nreverse results)
                 for child = (car (store other-class (list row)))
                 for record = (find id records :key #'dbq:id-of)
                 do (push child (slot-value record slot)))))
