(in-package :dbq)

(defgeneric pluralize (x)
  (:method (x)
    (pluralize (string-downcase x))))

(defmethod pluralize ((string string))
  (let* (flush add
               (len (length string))
               (last-char-raw (char string (1- len)))
               (last-char (char-upcase last-char-raw))
               (penult-char (char-upcase (if (cl:> len 1)
                                             (char string (- len 2))
                                             #\Nul))) ;dummy
               (last-3 (subseq string (max 0 (- len 3)))))
    (declare (character last-char-raw last-char penult-char)
             (string last-3))
    (setf (values flush add)
          (cond ((and (char-equal last-char #\Y)
                      (not (member penult-char '(#\A #\E #\I #\O #\U))))
                 (values 1 "ies"))
                ((or (string-equal string "ox")
                     (string-equal string "vax"))
                 (values nil "en"))
                ((or (and (char= last-char #\H)
                          (member penult-char '(#\C #\S)))
                     (member last-char '(#\S #\Z #\X)))
                 (values nil "es"))
                ((string-equal last-3 "man")
                 (values 2 "en"))
                ((string-equal last-3 "ife")
                 (values  2 "ves"))
                (t (values nil "s"))))
    (when flush
      (setq string (subseq string 0 (- len flush))))
    (concatenate 'string string add)))

(defgeneric singularize (x)
  (:method (x)
    (singularize (string-downcase x)))
  (:method ((string string))
    (cond ((alexandria:ends-with-subseq "ies" string)
           (ppcre:regex-replace "ies$" string "y"))
          ((alexandria:ends-with-subseq "IES" string)
           (ppcre:regex-replace "IES$" string "Y"))
          ((or (alexandria:ends-with #\s string)
               (alexandria:ends-with #\S string))
           (subseq string 0 (1- (length string))))
          (t
           string))))

(defun sym (&rest args)
  (intern
   (with-output-to-string (out)
     (loop for x in args
           do (write-string (string-upcase x) out)))
   *package*))

(defun key-sym (&rest args)
  (let ((*package* (find-package :keyword)))
    (apply #'sym args)))

(defun str (&rest args)
  (format nil "~{~a~}" args))


(defstruct location lat lng)

(defun read-geography-point (stream size)
  (declare (ignore size))
  (fast-io:with-fast-input (buffer nil stream)
    (let* ((le-p (= 1 (fast-io:readu8 buffer)))
           (r32 (if le-p #'fast-io:read32-le #'fast-io:read32-be))
           (r64 (if le-p #'fast-io:read64-le #'fast-io:read64-be))
           (type (funcall r32 buffer))
           (srid (funcall r32 buffer))
           (lng (ieee-floats:decode-float64 (funcall r64 buffer)))
           (lat (ieee-floats:decode-float64 (funcall r64 buffer))))
      (declare (ignore type srid))
      (make-location :lat lat :lng lng))))

;;; 環境によって oid が変わる？ もっといい方法ないかな？
(defun enable-read-geography-point ()
  (let ((typelem (cdaar (execute "select typelem from pg_type where typname='_geography'"))))
    (cl-postgres:set-sql-reader typelem #'read-geography-point :binary-p t)))

(defun column-name-to-slot-name (column-name)
  (sym (substitute #\- #\_ column-name)))

(defun column-name-to-accessor (column-name)
  (sym (column-name-to-slot-name column-name) "-OF"))

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(defun class-to-table-name (class)
  (substitute #\_ #\- (pluralize (string-downcase (class-name class)))))

(defgeneric to-table-name (x)
  (:method (x)
    (to-column-name x))
  (:method ((x symbol))
    (to-table-name (find-class x)))
  (:method ((x standard-class))
    (to-table-name (class-to-table-name x)))
  (:method ((x standard-object))
    (to-table-name (class-of x))))

(defgeneric to-column-name (x)
  (:method ((x string))
    x)
  (:method ((x symbol))
    (to-column-name (substitute #\_ #\- (string-downcase x)))))

(defgeneric to-sql-value (x)
  (:method ((x number))
    (princ-to-string x))
  (:method ((x string))
    (with-output-to-string (out)
      (write-char #\' out)
      (loop for c across x
            do (cond ((char= #\' c)
                      (write-string "''" out))
                     (t
                      (write-char c out))))
      (write-char #\' out)))
  (:method ((x local-time:timestamp))
    (local-time:format-timestring
     nil x :format '(#\'
                     (:year 4) #\/ (:month 2) #\/ (:day 2)
                     #\space
                     (:hour 2) #\: (:min 2) #\: (:sec 2)
                     #\')))
  (:method ((x list))
    (format nil "(~{~/dbq::val/~^, ~})" x))
  (:method ((location location))
    (cond ((and (stringp (location-lng location))
                (stringp (location-lat location)))
           (format nil "ST_GeographyFromText('SRID=4326;POINT(~a ~a)')"
                   (location-lng location) (location-lat location)))
          ((and (numberp (location-lng location))
                (numberp (location-lat location)))
           (format nil "ST_GeographyFromText('SRID=4326;POINT(~f ~f)')"
                   (location-lng location) (location-lat location)))
          (t
           (error "Invalid location value ~a!" location))))
  (:method ((x (eql t)))
    "'t'")
  (:method ((x null))
    "'f'")
  (:method ((x (eql :null)))
    "null"))

(defgeneric to-foreign-key (x)
  (:method  (x)
    (sym x "-ID")))
