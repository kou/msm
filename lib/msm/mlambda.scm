(define-module msm.mlambda
  (use srfi-1)
  (use srfi-11)
  (use gauche.parameter)
  (use msm.base)
  (export mlambda <mlambda>))
(select-module msm.mlambda)

(define mlambda-table (make-parameter #f))

(define-class <mlambda> ()
  ((proc :init-keyword :proc)
   (ext-var-values :init-keyword :ext-var-values)
   (ext-var-names :init-keyword :ext-var-names)
   (args :init-keyword :args)
   (body :init-keyword :body)
   (id :init-keyword :id :init-form (gensym))))

(define (macroexpand2 expr)
  (if (not (pair? expr))
    expr
    (let loop ((expr expr) (prev expr) (curr (macroexpand expr)))
      (cond ((eq? prev curr) expr)
            ((not (and (pair? curr) (identifier? (car curr)))) curr)
            (else
              (let ((new-prev (cons '%temporary-macro-header (cdr curr))))
                (eval `(define %temporary-macro-header ,(car curr)) (interaction-environment))
                (loop curr
                      new-prev
                      (macroexpand new-prev))))))))

(define-macro mlambda
  (lambda (args . body)
    (define (var-table-push! table bind-vars sym)
      (or (memq sym bind-vars)
          (hash-table-push! table sym #t)))
    (define (formals->var-names formals)
      (if (pair? formals)
        (cons (car formals) (formals->var-names (cdr formals)))
        (list formals)))
    (define (exp->ext-vars exp bind-vars free-var-table)
      (let ((exp (macroexpand2 exp)))
        (cond
          ((symbol? exp) (var-table-push! free-var-table bind-vars exp))
          ((pair? exp)
           (let ((head (cond ((identifier? (car exp))
                              (identifier->symbol (car exp)))
                             ((symbol? (car exp))
                              (car exp))
                             (else #f))))
             (case head
               ((quote %macroexpand %macroexpand-1) #f)
               ((and or if when unless begin)
                (like-func-form->ext-vars exp bind-vars free-var-table))
               ((quasiquote)
                (quasiquote->ext-vars (cdr exp) bind-vars free-var-table 0))
               ((cond)
                (cond->ext-vars exp bind-vars free-var-table))
               ((case)
                (case->ext-vars exp bind-vars free-var-table))
               ((set!)
                (set!->ext-vars exp bind-vars free-var-table))
               ((lambda)
                (lambda-family->ext-vars (cadr exp) (cddr exp)
                                         bind-vars free-var-table))
               ((let)
                (let* ((named-let? (symbol? (cadr exp)))
                       (binds ((if named-let? caddr cadr) exp))
                       (body ((if named-let? cdddr cddr) exp)))
                  (for-each
                    (cut exp->ext-vars <> bind-vars free-var-table)
                    (map cdr binds))
                  (lambda-family->ext-vars
                    (map car binds) body
                    (if named-let? (cons (cadr exp) bind-vars) bind-vars)
                    free-var-table)))
               ((let*)
                (let loop ((bind-vars bind-vars) (bind-forms (cadr exp)))
                  (if (null? bind-forms)
                    (lambda-family->ext-vars '() (cddr exp) bind-vars free-var-table)
                    (begin (exp->ext-vars (cadar bind-forms) bind-vars free-var-table)
                           (loop (cons (caar bind-forms) bind-vars)
                                 (cdr bind-forms))))))
               ((letrec)
                (let* ((bind-forms (cadr exp))
                       (body (cddr exp))
                       (var-names (map car bind-forms))
                       (var-exps (map cadr bind-forms))
                       (bind-vars (append var-names bind-vars)))
                  (for-each (cut exp->ext-vars <> bind-vars free-var-table) var-exps)
                  (lambda-family->ext-vars '() body bind-vars free-var-table)))
               ((receive)
                (exp->ext-vars (caddr exp) bind-vars free-var-table)
                (lambda-family->ext-vars (cadr exp) (cdddr exp)
                                         bind-vars free-var-table))
               (else
                 (for-each (cut exp->ext-vars <> bind-vars free-var-table)
                           exp)))))
          )))
    (define (quasiquote->ext-vars exp bind-vars free-var-table depth)
      (if (not (list? exp)) '()
        (case (car exp)
          ((unquote unquote-splicing)
           (if (= depth 0)
             (for-each (cut exp->ext-vars <> bind-vars free-var-table)
                       (cdr exp))
             (quasiquote->ext-vars (cadr exp) bind-vars free-var-table (- depth 1))))
          ((quasiquote)
           (quasiquote->ext-vars (cdr exp) bind-vars free-var-table (+ depth 1)))
          (else (for-each (cut quasiquote->ext-vars <> bind-vars free-var-table depth)
                          exp)))))
    (define (cond->ext-vars exp bind-vars free-var-table)
      (exp->ext-vars (cadr exp) bind-vars free-var-table)
      (for-each
        (lambda (clause)
          (for-each (cut exp->ext-vars <> bind-vars free-var-table)
                    (if (eq? 'else (car clause))
                      (cdr clause)
                      clause)))
        (cddr exp)))
    (define (case->ext-vars exp bind-vars free-var-table)
      (exp->ext-vars (cadr exp) einter free-var-table)
      (for-each
        (lambda (clause)
          (for-each (cut exp->ext-vars <> bind-vars free-var-table)
                    (cdr clause)))
        (cddr exp)))
    (define (set!->ext-vars exp bind-vars free-var-table)
      (if (symbol? (cadr exp))
        (for-each (cut exp->ext-vars <> bind-vars free-var-table) (cdr exp))
        (exp->ext-vars `((setter ,(caadr exp)) ,@(cdadr exp) ,(caddr exp))
                       bind-vars free-var-table)))
    (define (like-func-form->ext-vars exp bind-vars free-var-table)
      (for-each (cut exp->ext-vars <> bind-vars free-var-table)
                (cdr exp)))
    (define (lambda-family->ext-vars args body bind-vars free-var-table)
      (define (lambda-body->inter-defines body)
        (if (null? body) (values '() '())
          (let ((exp (car body)))
            (case (and (pair? exp) (car exp))
              ((define) (receive (defs body)
                                 (lambda-body->inter-defines (cdr body))
                                 (values (cons exp defs) body)))
              ((begin)
               (receive (defs others)
                        (lambda-body->inter-defines (cdr exp))
                        (if (null? others)
                          (receive (defs2 others)
                                   (lambda-body->inter-defines (cdr body))
                                   (values (append defs defs2) others))
                          (values defs (append others body)))))
                (else (values '() body))))))
      (let*-values (((inter-defines body)
                     (lambda-body->inter-defines body))
                    ((inter-define-names)
                     (map (lambda (x)
                            (if (symbol? (cadr x))
                              (cadr x)
                              (caadr x)))
                          inter-defines))
                    ((arg-vars) (formals->var-names args))
                    ((bind-vars) (append arg-vars inter-define-names bind-vars))
                    )
        (for-each
          (lambda (def)
            (if (symbol? (cadr def))
              (exp->ext-vars exp bind-vars free-var-table)
              (lambda-family->ext-vars
                (cadr def) (cddr def) bind-vars free-var-table)))
          inter-defines)
        (for-each (lambda (exp) (exp->ext-vars exp bind-vars free-var-table)) body)))

    (let* ((var-table (make-hash-table))
           (dmy (lambda-family->ext-vars args body '() var-table))
           (ext-vars (hash-table-map var-table (lambda (k v) k))))
      `(make <mlambda>
         :proc (lambda ,args ,@body)
         :ext-var-values (lambda () (list ,@ext-vars))
         :ext-var-names ',ext-vars
         :args ',args
         :body ',body))))

(define-method object-apply ((m <mlambda>) . args)
  (apply (ref m 'proc) args))

(define-method marshallable? ((obj <mlambda>))
  #t)

(define (dummy-obj->real-obj lst dummy real)
  (map (lambda (x)
         (if (eq? x dummy)
           real
           x))
       lst))

(define (cache-with mlamb generator post-handler)
  (parameterize
    ((mlambda-table (or (mlambda-table) (make-hash-table 'eq?))))
    (if (hash-table-exists? (mlambda-table) (ref mlamb 'id))
      (hash-table-get (mlambda-table) (ref mlamb 'id))
      (let ((dummy (gensym)))
        (hash-table-put! (mlambda-table) (ref mlamb 'id) dummy)
        (let ((ml (generator)))
          (hash-table-put! (mlambda-table) (ref ml 'id) ml)
          (post-handler ml dummy))))))

(define-method x->marshalized-object ((obj <mlambda>) table)
  (cache-with obj
    (lambda ()
      (let ((env (filter (compose marshallable? cadr)
                         (zip (ref obj 'ext-var-names)
                              ((ref obj 'ext-var-values))))))
        (make <mlambda>
          :proc #f
          :ext-var-values
          (map (lambda (x)
                 (x->marshalized-object (cadr x) table))
               env)
          :ext-var-names (map car env)
          :args (ref obj 'args)
          :body (ref obj 'body)
          :id (ref obj 'id))))
    (lambda (mlamb dummy)
      (set! (ref mlamb 'ext-var-values)
            (dummy-obj->real-obj (ref mlamb 'ext-var-values) dummy mlamb))
      mlamb)))

(define-method write-object ((obj <mlambda>) out)
  (format out "#,(<mlambda> ~s ~s ~s ~s ~s)"
          (ref obj 'ext-var-values)
          (ref obj 'ext-var-names)
          (ref obj 'args)
          (ref obj 'body)
          (ref obj 'id)))

(define-reader-ctor '<mlambda>
  (let ((table (make-marshal-table)))
    (lambda (ext-var-values ext-var-names args body id)
      (make <mlambda>
            :proc #f
            :ext-var-values ext-var-values
            :ext-var-names ext-var-names
            :args args
            :body body
            ;; :id id
            ))))

(define-method unmarshal-object ((obj <mlambda>) table)
  (cache-with obj
    (lambda ()
        (make <mlambda>
          :proc #f
          :ext-var-values #f
          :ext-var-names (ref obj 'ext-var-names)
          :args (ref obj 'args)
          :body (ref obj 'body)
          :id (ref obj 'id)))
    (lambda (mlamb dummy)
      (let ((ext-var-values
             (dummy-obj->real-obj (map (cut unmarshal-object <> table)
                                       (ref obj 'ext-var-values))
                                  dummy
                                  mlamb)))
        (set! (ref mlamb 'proc)
              (eval `(let ,(zip (ref obj 'ext-var-names)
                                (map (pa$ list 'quote) ext-var-values))
                       (lambda ,(ref obj 'args) ,@(ref obj 'body)))
                    (interaction-environment)))
        (set! (ref obj 'ext-var-values)
              (lambda () ext-var-values))
        mlamb))))

(provide "msm/mlambda")
