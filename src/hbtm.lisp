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
