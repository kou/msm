#!/usr/bin/env gosh

(use test.unit)
(use marshal)

(let ((table #f))
  (define-test-case "Marshal test"
    (setup
     (lambda () (set! table (make-marshal-table))))
    ("can marshalizable? test"
     (for-each (lambda (obj)
                 (assert-true (marshalizable? obj)))
               (list 1 1.0 'a "a" #t #f :a '() #()
                     (list 1 1.0 'a "a" #t #f :a '() #())
                     (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                     #(1 1.0 'a "a" #t #f :a '() #())
                     (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                     #('(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                     (list (lambda () #f))
                     (list 1.0 (lambda () #f) 'a "a")
                     )))
    ("can't marshalizable? test"
     (for-each (lambda (obj)
                 (assert-false (marshalizable? obj)))
               (list (lambda () #f)
                     (with-module marshal (make <reference-object>))
                     )))
    ("marshal/unmarshal test"
     (for-each (lambda (obj)
                 (assert-equal obj (unmarshal table
                                              (read-from-string
                                               (marshal table obj)))))
               (list 1 'abc "a" '(1) #()
                     (lambda () #f)
                     (make-hash-table)
                     (with-module marshal
                       (make <reference-object>
                         :ref 1
                         :table-id (id-of table)))
                     (list 1 (lambda (x) x) '(1)))))
    ("id-get/id-ref/id-exists? test"
     (assert-each (lambda (obj)
                    (assert-false (id-exists? table obj))
                    (let ((id (id-get table obj)))
                      (assert-equal obj (id-ref table id))
                      (assert-true (id-exists? table id))))
                  (list 1 1.0 'a "a"
                        (list 1 '#(1 'a "a") 3)
                        (lambda () #f))
                  :apply-if-can #f))
    
    ))
