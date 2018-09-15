(in-package :dbq.test)

(deftest save-and-find-by-id ()
  (let ((entry (make-instance 'entry :title "題名" :content "本文")))
    (is (dbq:save entry))
    (let ((id (dbq:.id entry)))
      (is (numberp id))
      (let ((entry (dbq:fetch-one (dbq:query 'entry (dbq:where :id id)) :class 'entry)))
        (describe entry)
        (is (equal id (dbq:.id entry)))
        (is (equal "題名" (.title entry)))
        (is (equal "本文" (.content entry)))))))

(deftest transaction-test ()
  (let ((entry (make-instance 'entry :title "a" :content "b")))
    (dbq:save entry)
    (dbq:with-transaction
      (setf (.title entry) "AA")
      (dbq:save entry)
      (dbq:rollback))
    (is (string= "a" (.title (dbq:find-by 'entry :id (dbq:.id entry)))))
    (dbq:with-transaction
      (setf (.title entry) "AA")
      (dbq:save entry))
    (is (string= "AA" (.title (dbq:find-by 'entry :id (dbq:.id entry)))))))

(deftest hbtm-test ()
  (let ((category1 (make-instance 'category :name "プログラミング"))
        (category2 (make-instance 'category :name "読書"))
        (entry (make-instance 'entry :title "題名" :content "本文")))
    (dbq:save category1)
    (dbq:save category2)
    (dbq:save entry)
    (dbq:execute (format nil "insert into category_entries values(~d, ~d)"
                         (dbq:.id category1) (dbq:.id entry)))
    (dbq:execute (format nil "insert into category_entries values(~d, ~d)"
                         (dbq:.id category2) (dbq:.id entry)))
    (is (= (dbq:.id entry)
           (dbq:.id (dbq:fetch-one (dbq:query 'entry (dbq:join 'categories)
                                       (dbq:where :categories.id (dbq:.id category1)))))))
    (is (equal (list (dbq:.id category1) (dbq:.id category2))
               (mapcar #'dbq:.id (.categories entry))))))

(deftest hbtm-insert-test ()
  (let* ((category1 (make-instance 'category :name "プログラミング"))
         (category2 (make-instance 'category :name "読書"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :categories (list category1 category2))))
    (dbq:save entry)
    (let ((entry (dbq:find-by 'entry :id (dbq:.id entry))))
      (is (equal (list (dbq:.id category1) (dbq:.id category2))
                 (mapcar #'dbq:.id (.categories entry)))))
    (is (dbq:id= entry
             (dbq:fetch-one (dbq:query 'entry
                              (dbq:join 'categories)
                              (dbq:where :categories.id (dbq:.id category1))))))))

(deftest has-many-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (dbq:save entry)
    (let ((entry (dbq:find-by 'entry :id (dbq:.id entry))))
      (is (equal (list (dbq:.id comment1) (dbq:.id comment2))
                 (mapcar #'dbq:.id (.comments entry)))))
    (is (dbq:id= entry
                 (dbq:fetch-one (dbq:query 'entry
                                  (dbq:join 'comments)
                                  (dbq:where :comments.id (dbq:.id comment1))))))
    (setf (.comments entry) (list comment1))
    (dbq:save entry)
    (is (equal (list (dbq:.id comment1))
               (mapcar #'dbq:.id (dbq:fetch (dbq:query 'comment
                                                (dbq:where :entry-id (dbq:.id entry)))))))))

(deftest has-many-query-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (dbq:save entry)
    (let* ((entry (dbq:find-by 'entry :id (dbq:.id entry))))
      (is (= 1 (length (dbq:fetch (dbq:query (.comments entry) (dbq:limit 1))))))
      (is (= 2 (length (dbq:fetch (dbq:query (.comments entry)))))))))

(deftest belongs-test ()
  (let* ((user (aprog1 (make-instance 'user :name "こねら")
                 (dbq:save it)))
         (entry (make-instance 'entry :title "題名" :content "本文")))
    (setf (.user entry) user)
    (is (= (dbq:.id user) (.user-id entry)))
    (dbq:save entry)
    (is (dbq:id= entry
                 (dbq:fetch-one (dbq:query 'entry
                                  (dbq:join 'user)
                                  (dbq:where :users.id (dbq:.id user))))))))

(deftest count-test ()
  (dbq:execute "delete from entries")
  (loop repeat 3 do (dbq:save (make-instance 'entry :title "題名" :content "本文")))
  (loop repeat 3 do (dbq:save (make-instance 'entry :title "ねこ" :content "ねねこ")))
  (is (= 3 (dbq:count (dbq:query 'entry (dbq:where :content "ねねこ"))))))

(deftest has-many-through--has-many-test ()
  (let* ((comment1 (make-instance 'comment :content "こめんと1"))
         (comment2 (make-instance 'comment :content "こめんと2"))
         (entry1 (make-instance 'entry :title "題名1" :content "本文1"
                                       :comments (list comment1)))
         (entry2 (make-instance 'entry :title "題名2" :content "本文2"
                                       :comments (list comment2)))
         (user (make-instance 'user :name "こねら"
                              :entries (list entry1 entry2))))
    (dbq:save user)
    (let ((user (dbq:find-by 'user :id (dbq:.id user))))
      (dbq:fetch (dbq:query 'user (dbq:join 'comments)))
      (dbq:fetch (dbq:query (.comments user))))))

(deftest has-many-through--belongs-to-test ()
  (let* ((user1 (make-instance 'user :name "こねら"))
         (user2 (make-instance 'user :name "おおねら"))
         (community1 (make-instance 'community :name "ねこねこくらぶ"))
         (community2 (make-instance 'community :name "おさかな会")))
    (dbq:save user1)
    (dbq:save user2)
    (dbq:save community1)
    (dbq:save community2)
    (let ((community-member1 (make-instance 'community-member
                                            :role "生きもの係"
                                            :user-id (dbq:.id user1)
                                            :community-id (dbq:.id community1)))
          (community-member2 (make-instance 'community-member
                                            :role "かいちょう"
                                            :user-id (dbq:.id user2)
                                            :community-id (dbq:.id community1))))
      (dbq:save community-member1)
      (dbq:save community-member2)

      (let* ((community1 (car (dbq:fetch (dbq:query 'community
                                           (dbq:where :id (dbq:.id community1))))))
             (users (dbq:fetch (dbq:query (.users community1)))))
        (is users))
      (let ((community1 (car (dbq:fetch (dbq:query 'community
                                          (dbq:where :communities.id (dbq:.id community1))
                                          (dbq:join 'users))))))
        (is (= 2 (length (.users community1))))))))

(deftest preload-has-many-test ()
  (let* ((comment1 (make-instance 'comment :content "こんにちは"))
         (comment2 (make-instance 'comment :content "こんばんは"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :comments (list comment1 comment2))))
    (dbq:save entry)
    (let ((entry (car (dbq:fetch (dbq:query 'entry
                                   (dbq:where :id (dbq:.id entry))
                                   (dbq:preload 'comments))))))
      (is (slot-boundp entry 'comments))
      (is (= 2 (length (slot-value entry 'comments)))))))

(deftest preload-hbtm-test ()
  (let* ((category1 (make-instance 'category :name "プログラミング"))
         (category2 (make-instance 'category :name "読書"))
         (entry (make-instance 'entry :title "題名" :content "本文"
                                      :categories (list category1 category2))))
    (dbq:save entry)
    (let ((entry (car (dbq:fetch (dbq:query 'entry
                                   (dbq:where :id (dbq:.id entry))
                                   (dbq:preload 'categories))))))
      (is (slot-boundp entry 'categories))
      (is (= 2 (length (slot-value entry 'categories)))))))

(deftest preload-belongs-test ()
  (let* ((user (make-instance 'user :name "こねら"))
         (entry (make-instance 'entry :title "題名" :content "本文")))
    (dbq:save user)
    (setf (.user entry) user)
    (dbq:save entry)
    (let ((entry (car (dbq:fetch (dbq:query 'entry
                                   (dbq:where :id (dbq:.id entry))
                                   (dbq:preload 'user))))))
      (is (slot-boundp entry 'user)))))

(deftest preload-multi-tables-test ()
  (let* ((comment1 (make-instance 'comment :content "a1"))
         (comment2 (make-instance 'comment :content "a2"))
         (comment3 (make-instance 'comment :content "b1"))
         (comment4 (make-instance 'comment :content "b2"))
         (entry1 (make-instance 'entry :title "a" :content "aa"
                                       :comments (list comment1 comment2)))
         (entry2 (make-instance 'entry :title "b" :content "bb"
                                       :comments (list comment3 comment4)))
         (user (make-instance 'user :name "こねら"
                                    :entries (list entry1 entry2))))
    (dbq:save user)
    (let ((user (car (dbq:fetch (dbq:query 'user
                                  (dbq:where :id (dbq:.id user))
                                  (dbq:preload '(entries comments)))))))
      (is (slot-boundp user 'entries))
      (is (slot-boundp (car (.entries user)) 'comments))
      (is (= 2 (length (.comments (car (.entries user)))) )))))

(run-package-tests :interactive t)
