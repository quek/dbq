(in-package :dbq.test)

(deftest save-and-find-by-id ()
  (let ((entry (make-instance 'entry :title "題名" :content "本文")))
    (is (save entry))
    (let ((id (id-of entry)))
      (is (numberp id))
      (let ((entry (fetch-one (query 'entry (where :id id)) :class 'entry)))
        (describe entry)
        (is (equal id (id-of entry)))
        (is (equal "題名" (title-of entry)))
        (is (equal "本文" (content-of entry)))))))

(deftest transaction-test ()
  (let ((entry (make-instance 'entry :title "a" :content "b")))
    (save entry)
    (with-transaction
      (setf (title-of entry) "AA")
      (save entry)
      (rollback))
    (is (string= "a" (title-of (find-by 'entry :id (id-of entry)))))
    (with-transaction
      (setf (title-of entry) "AA")
      (save entry))
    (is (string= "AA" (title-of (find-by 'entry :id (id-of entry)))))))

(deftest hbtm-test ()
  (let ((category1 (make-instance 'category :name "プログラミング"))
        (category2 (make-instance 'category :name "読書"))
        (entry (make-instance 'entry :title "題名" :content "本文")))
    (save category1)
    (save category2)
    (save entry)
    (execute (format nil "insert into category_entries values(~d, ~d)"
                     (id-of category1) (id-of entry)))
    (execute (format nil "insert into category_entries values(~d, ~d)"
                     (id-of category2) (id-of entry)))
    (is (= (id-of entry)
           (id-of (fetch-one (query 'entry (join 'categories)
                               (where :categories.id (id-of category1)))))))
    (is (equal (list (id-of category1) (id-of category2))
               (mapcar #'id-of (categories-of entry))))))

(deftest hbtm-insert-test ()
  (let* ((category1 (make-instance 'category :name "プログラミング"))
         (category2 (make-instance 'category :name "読書"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :categories (list category1 category2))))
    (save entry)
    (let ((entry (find-by 'entry :id (id-of entry))))
      (is (equal (list (id-of category1) (id-of category2))
                 (mapcar #'id-of (categories-of entry)))))
    (is (id= entry
             (fetch-one (query 'entry
                          (join 'categories)
                          (where :categories.id (id-of category1))))))))

(deftest has-many-insert-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (save entry)
    (let ((entry (find-by 'entry :id (id-of entry))))
      (is (equal (list (id-of comment1) (id-of comment2))
                 (mapcar #'id-of (comments-of entry)))))
    (is (id= entry
             (fetch-one (query 'entry
                          (join 'comments)
                          (where :comments.id (id-of comment1))))))
    (setf (comments-of entry) (list comment1))
    (save entry)
    (is (equal (list (id-of comment1))
               (mapcar #'id-of (fetch (query 'comment (where :entry-id (id-of entry)))))))))

(deftest belongs-test ()
  (let* ((user (aprog1 (make-instance 'user :name "こねら")
                 (save it)))
         (entry (make-instance 'entry :title "題名" :content "本文")))
    (setf (user-of entry) user)
    (is (= (id-of user) (user-id-of entry)))
    (save entry)
    (is (id= entry
             (fetch-one (query 'entry
                          (join 'user)
                          (where :users.id (id-of user))))))))

(deftest count-test ()
  (execute "delete from entries")
  (loop repeat 3 do (save (make-instance 'entry :title "題名" :content "本文")))
  (loop repeat 3 do (save (make-instance 'entry :title "ねこ" :content "ねねこ")))
  (is (= 3 (dbq:count (query 'entry (where :content "ねねこ")) ))))

(run-package-tests :interactive t)
