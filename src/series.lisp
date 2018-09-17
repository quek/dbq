(in-package :dbq)

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
