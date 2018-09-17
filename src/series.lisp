(in-package :dbq)

#+nil
(series::defun scan-query (query)
  (declare (series::optimizable-series-function))
  (series::producing  (z) ((xs (fetch query)) x)
    (loop
      (tagbody
         (when (endp xs)
           (series::terminate-producing))
         (setq x (car xs))
         (setq xs (cdr xs))
         (series:next-out z x)))))

(series::defS scan-query (query)
  "(scan-query (query 'prefecture))"
  (series::fragl ((query))                 ;args
                 ((x t))                   ;rets
                 ((x t) (xs list (fetch query))) ;aux
                 ()                        ;alt
                 ()                        ;prolog
                 ((when (endp xs)          ;body
                    (go series::end))
                  (setq x (car xs))
                  (setq xs (cdr xs)))
                 ()                     ;epilog
                 ()                     ;wraprs
                 nil)                   ;impure
  :optimizer
  (series::apply-literal-frag
   `((((query))                         ; args
                ((x t))                 ; rets
                ((x t) (xs list (fetch query))) ; aux
                ()                              ; alt
                ()                              ; prolog
                (; body:
                 (unless xs (go series::end))
                 (setq x (car xs))
                 (setq xs (cdr xs)))
                () ; epilog
                (); wraprs:
                nil ; impure
                )
     ,query
     )))

#+nil
(series::defS scan-query (query)
  "(scan-query (query 'prefecture))"
  (series::efragl ((query))                 ;args
                  `(((x t))                  ;rets
                            ((x t) (xs list (fetch query))) ;aux
                            ()                              ;alt
                            ()                              ;prolog
                            ((when (endp xs)                ;body
                               (go series::end))
                             (setq x (car xs))
                             (setq xs (cdr xs)))
                            ()           ;epilog
                            ()           ;wraprs
                            nil))        ;impure
  )
