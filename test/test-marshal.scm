#!/usr/bin/env gosh

(define-module msm-test
  (extend msm.marshal)
  (use test.unit)
  (use srfi-19))
(select-module msm-test)

(let ((table #f)
      (sorter (cut sort <> (lambda (x y)
                             (< (car x) (car y))))))
  (define-test-case "Marshal test"
    (setup
     (lambda () (set! table (make-marshal-table))))
    ("can marshallable? test"
     (assert-each (lambda (obj)
                    (assert-true (marshallable? obj)
                                 (format #f " <~a> must be marshallable" obj)))
                  (list 1 1.0 'a "a" #t #f :a '() #()
                        #/reg/ (string->regexp "reg")
                        (list 1 1.0 'a "a" #t #f :a '() #())
                        (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                        #(1 1.0 'a "a" #t #f :a '() #())
                        (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                        #('(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                        (list (lambda () #f))
                        (list 1.0 (lambda () #f) 'a "a")
                        (list 1.0
                              (make <reference-object> :ref 100 :table-id 1))
                        (current-date))
                  :apply-if-can #f))
    ("can't marshallable? test"
     (assert-each (lambda (obj)
                    (assert-false (marshallable? obj)
                                  (format #f
                                          " <~a> must be not marshallable"
                                          obj)))
                  (list (lambda () #f)
                        (make <reference-object> :ref 100 :table-id 1))
                  :apply-if-can #f))
    ("marshal/unmarshal test"
     (assert-each (lambda (obj)
                    (assert-equal obj (unmarshal table
                                                 (read-from-string
                                                  (marshal table obj)))))
                  (list 1 'abc "a" '(1) #()
                        (lambda () #f)
                        (make-hash-table)
                        (let* ((id 1)
                               (ref (make <reference-object>
                                      :ref id
                                      :table-id (id-of table))))
                          (id-put! table id ref)
                          ref)
                        (make <reference-object>
                          :ref 10
                          :table-id (id-of (make-marshal-table)))
                        (list 1 (lambda (x) x) '(1))
                        (current-date))
                  :apply-if-can #f))
    ("id-get/id-ref/id-remove!/id-exists? test"
     (assert-each (lambda (obj)
                    (assert-false (id-exists? table obj))
                    (let ((id (id-get table obj)))
                      (assert-equal obj (id-ref table id))
                      (assert-true (id-exists? table id))
                      (assert-true (id-delete! table id))
                      (assert-false (id-delete! table id #f))
                      (assert-false (id-exists? table id))))
                  (list 1 1.0 'a "a"
                        (list 1 '#(1 'a "a") 3)
                        (lambda () #f))
                  :apply-if-can #f))
    ("id-put! test"
     (assert-each (lambda (id obj)
                    (assert-false (id-exists? table id))
                    (id-put! table id obj)
                    (assert-true (id-exists? table id))
                    (assert-equal obj (id-ref table id)))
                  `((1 "str")
                    (2 123)
                    (3 abc)
                    (4 :abc)
                    (5 (1 2 3)))))
    ("id-put! test when obj is #f"
     (let ((id 100))
       (assert-false (id-exists? table id))
       (id-put! table id #f)
       (assert-false (id-exists? table id))))
    ("id-delete! test when id is not associated"
     (assert-each (lambda (obj)
                    (assert-error (lambda () (id-delete! table 0)))
                    (assert-equal obj (id-delete! table 0 obj)))
                  (list 1 1.0 'a "a"
                        (list 1 '#(1 'a "a") 3)
                        (lambda () #f))
                  :apply-if-can #f))
    ("*marshal-false-id* test"
     (assert-false (id-ref table *marshal-false-id*)))
    ("id-fold test"
     (let ((folder (lambda (key value r)
                     (cons (list key value)
                           r))))
       (assert-equal '() (id-fold table folder '()))
       (let ((lst '((1 "str")
                    (2 (1 2 3))
                    (3 abc)
                    (4 :key))))
         (for-each (cut apply id-put! table <>) lst)
         (assert-equal (sorter lst)
                       (sorter (id-fold table folder '()))))))
    ("id-for-each test"
     (let ((tested? #f)
           (lst '((1 "str")
                  (2 (1 2 3))
                  (3 abc)
                  (4 :key))))
       (for-each (cut apply id-put! table <>) lst)
       (id-for-each table
                    (lambda (key value)
                      (set! tested? #t)
                      (let ((target (assq key lst)))
                        (assert-equal (car target) key)
                        (assert-equal (cadr target) value))))
       (assert-true tested?)))
    ("id-map test"
     (let ((mapper (lambda (id obj)
                     (list (+ 100 id)
                           (list obj))))
           (lst '((1 "str")
                  (2 (1 2 3))
                  (3 abc)
                  (4 :key))))
       (for-each (cut apply id-put! table <>) lst)
       (assert-equal (sorter (map (cut apply mapper <>) lst))
                     (sorter (id-map table mapper)))))
    ("alist->marshal-table/marshal-table->alist test"
     (assert-each (lambda (alist)
                    (assert-equal (sorter alist)
                                  (sorter (marshal-table->alist
                                           (alist->marshal-table alist)))))
                  '(()
                    ((1 . "str")
                     (2 . (1 2 3))
                     (3 . abc)
                     (4 . :key)))
                  :apply-if-can #f))))
