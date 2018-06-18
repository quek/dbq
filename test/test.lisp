(in-package :dbq.test)

(deftest save-and-find-by-id ()
  (let ((entry (make-instance 'entry :title "題名" :content "本文")))
    (is (dbq:save entry))
    (let ((id (dbq:id-of entry)))
      (is (numberp id))
      (let ((entry (dbq:fetch-one (dbq:query 'entry (dbq:where :id id)) :class 'entry)))
        (describe entry)
        (is (equal id (dbq:id-of entry)))
        (is (equal "題名" (title-of entry)))
        (is (equal "本文" (content-of entry)))))))

(deftest transaction-test ()
  (let ((entry (make-instance 'entry :title "a" :content "b")))
    (dbq:save entry)
    (dbq:with-transaction
      (setf (title-of entry) "AA")
      (dbq:save entry)
      (dbq:rollback))
    (is (string= "a" (title-of (dbq:find-by 'entry :id (dbq:id-of entry)))))
    (dbq:with-transaction
      (setf (title-of entry) "AA")
      (dbq:save entry))
    (is (string= "AA" (title-of (dbq:find-by 'entry :id (dbq:id-of entry)))))))

(deftest hbtm-test ()
  (let ((category1 (make-instance 'category :name "プログラミング"))
        (category2 (make-instance 'category :name "読書"))
        (entry (make-instance 'entry :title "題名" :content "本文")))
    (dbq:save category1)
    (dbq:save category2)
    (dbq:save entry)
    (dbq:execute (format nil "insert into category_entries values(~d, ~d)"
                         (dbq:id-of category1) (dbq:id-of entry)))
    (dbq:execute (format nil "insert into category_entries values(~d, ~d)"
                         (dbq:id-of category2) (dbq:id-of entry)))
    (is (= (dbq:id-of entry)
           (dbq:id-of (dbq:fetch-one (dbq:query 'entry (dbq:join 'categories)
                                       (dbq:where :categories.id (dbq:id-of category1)))))))
    (is (equal (list (dbq:id-of category1) (dbq:id-of category2))
               (mapcar #'dbq:id-of (categories-of entry))))))

(deftest hbtm-insert-test ()
  (let* ((category1 (make-instance 'category :name "プログラミング"))
         (category2 (make-instance 'category :name "読書"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :categories (list category1 category2))))
    (dbq:save entry)
    (let ((entry (dbq:find-by 'entry :id (dbq:id-of entry))))
      (is (equal (list (dbq:id-of category1) (dbq:id-of category2))
                 (mapcar #'dbq:id-of (categories-of entry)))))
    (is (dbq:id= entry
             (dbq:fetch-one (dbq:query 'entry
                              (dbq:join 'categories)
                              (dbq:where :categories.id (dbq:id-of category1))))))))

(deftest has-many-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (dbq:save entry)
    (let ((entry (dbq:find-by 'entry :id (dbq:id-of entry))))
      (is (equal (list (dbq:id-of comment1) (dbq:id-of comment2))
                 (mapcar #'dbq:id-of (comments-of entry)))))
    (is (dbq:id= entry
                 (dbq:fetch-one (dbq:query 'entry
                                  (dbq:join 'comments)
                                  (dbq:where :comments.id (dbq:id-of comment1))))))
    (setf (comments-of entry) (list comment1))
    (dbq:save entry)
    (is (equal (list (dbq:id-of comment1))
               (mapcar #'dbq:id-of (dbq:fetch (dbq:query 'comment
                                                (dbq:where :entry-id (dbq:id-of entry)))))))))

(deftest has-many-query-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (dbq:save entry)
    (let* ((entry (dbq:find-by 'entry :id (dbq:id-of entry))))
      (is (= 1 (length (dbq:fetch (dbq:query (comments-of entry) (dbq:limit 1))))))
      (is (= 2 (length (dbq:fetch (dbq:query (comments-of entry)))))))))

(deftest belongs-test ()
  (let* ((user (aprog1 (make-instance 'user :name "こねら")
                 (dbq:save it)))
         (entry (make-instance 'entry :title "題名" :content "本文")))
    (setf (user-of entry) user)
    (is (= (dbq:id-of user) (user-id-of entry)))
    (dbq:save entry)
    (is (dbq:id= entry
                 (dbq:fetch-one (dbq:query 'entry
                                  (dbq:join 'user)
                                  (dbq:where :users.id (dbq:id-of user))))))))

(deftest count-test ()
  (dbq:execute "delete from entries")
  (loop repeat 3 do (dbq:save (make-instance 'entry :title "題名" :content "本文")))
  (loop repeat 3 do (dbq:save (make-instance 'entry :title "ねこ" :content "ねねこ")))
  (is (= 3 (dbq:count (dbq:query 'entry (dbq:where :content "ねねこ"))))))

(deftest has-many-through-test ()
  (let* ((comment1 (make-instance 'comment :content "こめんと1"))
         (comment2 (make-instance 'comment :content "こめんと2"))
         (entry1 (make-instance 'entry :title "題名1" :content "本文1"
                                       :comments (list comment1)))
         (entry2 (make-instance 'entry :title "題名2" :content "本文2"
                                       :comments (list comment2)))
         (user (make-instance 'user :name "こねら"
                              :entries (list entry1 entry2))))
    (dbq:save user)
    (let ((user (dbq:find-by 'user :id (dbq:id-of user))))
      (dbq:fetch (dbq:query 'user (dbq:join 'comments)))
      (dbq:fetch (dbq:query (comments-of user))))))

(run-package-tests :interactive t)
