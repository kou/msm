#!/usr/bin/env gosh

(use test.unit)
(use gauche.parameter)
(use msm.marshal)
(use srfi-19)

(define a (make-parameter 0))
(define b (make-parameter 1))

(let ((table #f)
      (sorter (cut sort <> (lambda (x y)
                             (< (car x) (car y)))))
      (<reference-object> (with-module msm.marshal <reference-object>)))
  (define-test-case "Marshal test"
    (setup
     (lambda () (set! table (make-marshal-table))))
    ("can marshallable? test"
     (assert-each (lambda (obj)
                    (assert-true (marshallable? obj)
                                 (format #f " <~a> must be marshallable" obj)))
                  (list (mlambda () #f))
                  :apply-if-can #f))
    ("marshal/unmarshal test"
     (assert-each (lambda (proc args)
                    (let ((mproc (unmarshal
                                  table
                                  (read-from-string
                                   (marshal table proc)))))
                      (assert-equal (apply proc args)
                                    (apply mproc args))))
                  (list
                   (let ((x 0) (y 1))
                     (list (mlambda () (list x y))
                           '()))
                   (list (letrec ((f (mlambda (x)
                                       (if (zero? x)
                                         1
                                         (* x (f (- x 1)))))))
                           f)
                         '(4)))
                  :apply-if-can #t))
    ("different environment test"
     (let* ((m (mlambda () (list (a) (b))))
            (proc (unmarshal
                    table
                    (read-from-string
                      (marshal table m)))))
       (a 0)
       (b 1)
       (assert-equal (proc) '(0 1))))
    ))
