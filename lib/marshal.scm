(define-module marshal
  (use srfi-1)
  (use srfi-10)
  (use srfi-19)
  (use util.list)
  (use math.mt-random)
  (use gauche.collection)
  (export *marshal-version*
          marshalizable? reference-object? using-same-table?
          marshal unmarshal
          id-get id-put! id-ref id-delete! id-exists?
          id-fold id-for-each id-map
          marshal-table->alist alist->marshal-table
          make-marshal-table
          *marshal-false-id*)
  )
(select-module marshal)

(define *marshal-version* "0.0.2")

(define mt-random (make <mersenne-twister> :seed (sys-time)))
(define-method random ()
  (mt-random-real mt-random))
(define-method random ((max <integer>))
  (mt-random-integer mt-random max))

(define-class <reference-object> ()
  ((ref :init-keyword :ref :accessor ref-of)
   (table-id :init-keyword :table-id :accessor table-id-of)))

(define (reference-object? obj)
  (is-a? obj <reference-object>))
         
(define-reader-ctor '<reference-object>
  (lambda args
    (apply make <reference-object> args)))

(define-method write-object ((self <reference-object>) out)
  (format out "#,(<reference-object> :ref ~s :table-id ~s)"
          (ref-of self)
          (table-id-of self)))

(define-method object-hash ((self <reference-object>))
  (logior (hash (ref-of self))
          (hash (table-id-of self))))

(define (equal-reference-object? ref1 ref2)
  (and (= (ref-of ref1) (ref-of ref2))
       (= (table-id-of ref1) (table-id-of ref2))))

(define-method object-equal? ((self <reference-object>) other)
  (and (is-a? other <reference-object>)
       (equal-reference-object? self other)))

(define-method object-equal? (self (other <reference-object>))
  (and (is-a? other <reference-object>)
       (equal-reference-object? self other)))

(define-class <marshal-table> ()
  ((id :accessor id-of :init-form (random))
   (obj->id :accessor obj->id-of :init-form (make-hash-table 'eq?))
   (id->obj :accessor id->obj-of :init-form (make-hash-table 'eqv?))
   (size :accessor size-of :init-value 0))
  )

(define (make-marshal-table)
  (make <marshal-table>))

(define (generate-new-id table)
  (define max (+ 65535 (expt (size-of table) 2)))
  (define (generate)
    (+ 1 (random max)))
  (define (loop)
    (let ((new-id (generate)))
      (if (id-exists? table new-id)
          (loop)
          new-id)))
  (loop))

(define *marshal-false-id* 0)

(define-method id-get ((table <marshal-table>) obj)
  (if (eq? obj #f)
      *marshal-false-id*
      (or (hash-table-get (obj->id-of table) obj #f)
          (let ((new-id (generate-new-id table)))
            (hash-table-put! (obj->id-of table) obj new-id)
            (hash-table-put! (id->obj-of table) new-id obj)
            (inc! (size-of table))
            new-id))))

(define-method id-put! ((table <marshal-table>) id obj)
  (when (and (not (eq? obj #f))
             (not (id-exists? table id)))
    (hash-table-put! (obj->id-of table) obj id)
    (hash-table-put! (id->obj-of table) id obj)
    id))

(define-method id-ref ((table <marshal-table>) id . fallback)
  (if (= id *marshal-false-id*)
      #f
      (or (hash-table-get (id->obj-of table) id #f)
          (if (null? fallback)
              (error "no object with id: " id)
              (car fallback)))))

(define-method id-delete! ((table <marshal-table>) id . fallback)
  (if (id-exists? table id)
      (let ((obj (id-get table id)))
        (hash-table-delete! (obj->id-of table) obj)
        (hash-table-delete! (id->obj-of table) id)
        #t)
      (if (null? fallback)
          (error "no object with id: " id)
          (car fallback))))

(define-method id-exists? ((table <marshal-table>) id)
  (hash-table-exists? (id->obj-of table) id))

(define-method id-fold ((table <marshal-table>) proc knil)
  (hash-table-fold (id->obj-of table) proc knil))

(define-method id-for-each ((table <marshal-table>) proc)
  (hash-table-for-each (id->obj-of table) proc))

(define-method id-map ((table <marshal-table>) proc)
  (hash-table-map (id->obj-of table) proc))

(define (marshal-table->alist table)
  (hash-table->alist (id->obj-of table)))

(define (alist->marshal-table alist)
  (let ((table (make-marshal-table)))
    (for-each (lambda (elem)
                (id-put! table (car elem) (cdr elem)))
              alist)
    table))

(define-method marshalizable? (obj)
  #f)

(define-method marshalizable? ((obj <number>))
  #t)

(define-method marshalizable? ((obj <symbol>))
  #t)

(define-method marshalizable? ((obj <char>))
  #t)

(define-method marshalizable? ((obj <string>))
  #t)

(define-method marshalizable? ((obj <boolean>))
  #t)

(define-method marshalizable? ((obj <keyword>))
  #t)

(define-method marshalizable? ((lst <list>))
  #t)

(define-method marshalizable? ((vec <vector>))
  #t)

(define-method marshalizable? ((date <date>))
  #t)

(define-reader-ctor '<date>
  (lambda args
    (apply make-date args)))

(define-method write-object ((self <date>) out)
  (format out "#,(<date> ~s ~s ~s ~s ~s ~s ~s ~s)"
          (date-nanosecond self)
          (date-second self)
          (date-minute self)
          (date-hour self)
          (date-day self)
          (date-month self)
          (date-year self)
          (date-zone-offset self)))

(define-method object-equal? ((self <date>) (other <date>))
  (and (equal? (date-nanosecond self)
               (date-nanosecond other))
       (equal? (date-second self)
               (date-second other))
       (equal? (date-minute self)
               (date-minute other))
       (equal? (date-hour self)
               (date-hour other))
       (equal? (date-day self)
               (date-day other))
       (equal? (date-month self)
               (date-month other))
       (equal? (date-year self)
               (date-year other))
       (equal? (date-zone-offset self)
               (date-zone-offset other))))

(define (using-same-table? table object)
  (and (reference-object? object)
       (= (id-of table) (table-id-of object))))

(define (make-reference-object-from-marshal-table table obj)
  (make <reference-object>
    :ref (id-get table obj)
    :table-id (id-of table)))

(define (marshal table object)
  (define (make-marshalized-object obj)
    (if (and (marshalizable? obj)
             (is-a? obj <collection>))
        (map-to (class-of obj)
                make-marshalized-object
                obj)
        (if (or (marshalizable? obj)
                (and (reference-object? obj)
                     (not (using-same-table? table obj))))
            obj
            (make-reference-object-from-marshal-table table obj))))

  (let ((out (open-output-string)))
    (write (make-marshalized-object object) out)
    (get-output-string out)))

(define (unmarshal table object)
  (define (rec obj)
    (if (is-a? obj <collection>)
        (map-to (class-of obj)
                rec
                obj)
        (if (using-same-table? table obj)
            (id-ref table (ref-of obj))
            obj)))

  (rec object))

(provide "marshal")