#!/usr/bin/env gosh

(use benchmark)
(use marshal)

(define n 1000)

(define-syntax n-times
  (syntax-rules ()
    ((_ body ...)
     (lambda ()
       (dotimes (i n)
         body ...)))))

(define (main arg)
  (let ((table (make-marshal-table)))
    (bm (lambda (r)
          (report r (n-times (marshal table (lambda () 1)))
                  :label "1 lambda")
          (let ((obj (marshal table (lambda () 1))))
            (report r (n-times (unmarshal table obj))
                    :label "1 unmarshal"))
          (report r (n-times (marshal table 1))
                  :label "1")
          (let ((obj (marshal table 1)))
            (report r (n-times (unmarshal table obj))
                    :label "1"))
          (report r (n-times (marshal table (lambda () (lambda () 1))))
                  :label "2 lambdas")
          (let ((obj (marshal table (lambda () (lambda () 1)))))
            (report r (n-times (unmarshal table obj))
                    :label "2 unmarshal")))
        :label-width 15)))
