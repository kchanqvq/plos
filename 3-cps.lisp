(defpackage #:plos
  (:use #:cl)
  (:shadow #:eval #:apply))
(in-package #:plos)

(defun tagged-list? (obj tag)
  (and (consp obj) (eq (car obj) tag)))

(defun lookup (env symbol)
  (if (consp env)
      (let ((cell (assoc symbol (car env))))
        (if cell (cdr cell)
            (lookup (cdr env) symbol)))
      (error "Undefined variable ~S." symbol)))

(defun define (env symbol value)
  (if (consp env)
      (let ((cell (assoc symbol (car env))))
        (if cell (setf (cdr cell) value)
            (setf (car env)
                  (cons (cons symbol value)
                        (car env))))
        value)
      (error "Cannot define ~S in null environment." symbol)))

(defun eval (form env cont)
  (cond
    ((symbolp form) (funcall cont (lookup env form)))
    ((tagged-list? form 'quote) (funcall cont (cdr form)))
    ((tagged-list? form 'if)
     (eval (cadr form) env
           (lambda (test)
             (if test
                 (eval (caddr form) env cont)
                 (eval (cadddr form) env cont)))))
    ((tagged-list? form 'progn)
     (eval-multiple (cdr form) env cont))
    ((tagged-list? form 'lambda)
     (funcall cont (list* 'closure env (cadr form) (cddr form))))
    ((tagged-list? form 'define)
     (eval (caddr form) env
           (lambda (value)
             (funcall cont (define env (cadr form) value)))))
    ((tagged-list? form 'load)
     (eval-multiple (uiop:read-file-forms (cadr form)) env cont))
    ((consp form)
     (mapeval form nil env
              (lambda (values)
                (apply (car values) (cdr values) cont))))
    (t (funcall cont form))))

(defun mapeval (forms results env cont)
  (if (consp forms)
      (eval (car forms) env
            (lambda (result)
              (mapeval (cdr forms) (cons result results) env cont)))
      (funcall cont (reverse results))))

(defun eval-multiple (forms env cont)
  "Evaluate FORMS one by one in ENV.
Return the value of the last form."
  (if (cdr forms)
      (eval (car forms) env
            (lambda (result)
              (eval-multiple (cdr forms) env cont)))
      (eval (car forms) env cont)))

(defun apply (op args cont)
  (cond
    ((functionp op) (funcall cont (cl:apply op args)))
    ((tagged-list? op 'closure)
     (eval-multiple (cdddr op)
                    (cons (mapcar #'cons (caddr op) args)
                          (cadr op))
                    cont))
    (t (error "~S is not applicable." op))))

(defun boot ()
  (let ((*print-circle* t)
        (env
          (list
           (list (cons nil nil)
                 (cons t t)
                 (cons 'cons? #'consp)
                 (cons 'cons #'cons)
                 (cons 'car #'car)
                 (cons 'cdr #'cdr)
                 (cons 'list #'list)
                 (cons 'list* #'list*)
                 (cons 'set-car! #'rplaca)
                 (cons 'set-cdr! #'rplacd)
                 (cons 'cadr #'cadr)
                 (cons 'cddr #'cddr)
                 (cons 'caddr #'caddr)
                 (cons 'cdddr #'cdddr)
                 (cons 'number? #'numberp)
                 (cons '+ #'+)
                 (cons '- #'-)
                 (cons '* #'*)
                 (cons '/ #'/)
                 (cons '< #'<)
                 (cons '> #'>)
                 (cons 'print (lambda (x) (prin1 x) (terpri)))
                 (cons 'error! #'error)
                 (cons 'call/cc 'call/cc)))))
    (handler-case
        (loop
          (format t "~&PLOS-EVAL> ")
          (eval (read) env #'prin1))
      (end-of-file () (format t "~%Moriturus te saluto.")))))
