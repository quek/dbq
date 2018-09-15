(in-package :dbq)

(defun update-has-many (record)
  "新しいリストにないものは削除してみよう。
問題があるようなら _delete スロット追加とか検討する。"
  (loop with id = (.id record)
        with class = (class-name (class-of record))
        for slot in (has-many-slots class)
        for reldat = (reldat class slot)
        for other-class = (slot-value reldat 'other-class)
        for foreign-key-slot = (slot-value reldat 'foreign-key)
        for through = (slot-value reldat 'through)
        if (and (null through) (slot-boundp record slot))
          do (let ((old-list (fetch (query other-class (where foreign-key-slot id)))))
              (loop for x in (slot-value record slot)
                    do (setf (slot-value x (slot-value reldat 'foreign-key))
                             id)
                       (save x)
                       (setf old-list (cl:delete x old-list :test #'id=)))
               (loop for old in old-list
                     do (delete old)))))
