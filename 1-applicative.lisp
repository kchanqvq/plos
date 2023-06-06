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

(defun eval (form env)
  (cond
    ((symbolp form) (lookup env form))
    ((tagged-list? form 'quote) (cadr form))
    ((tagged-list? form 'if)
     (if (eval (cadr form) env)
         (eval (caddr form) env)
         (eval (cadddr form) env)))
    ((consp form)
     (let ((values (mapcar (lambda (arg) (eval arg env)) form)))
       (apply (car values) (cdr values))))
    (t form)))

(defun apply (op args)
  (cond
    ((functionp op) (cl:apply op args))
    (t (error "~S is not applicable." op))))

(defun boot ()
  (let ((env
          (list
           (list (cons 'cons? #'consp)
                 (cons 'cons #'cons)
                 (cons 'car #'car)
                 (cons 'cdr #'cdr)
                 (cons 'list #'list)
                 (cons 'list* #'list*)
                 (cons 'number? #'numberp)
                 (cons '+ #'+)
                 (cons '- #'-)
                 (cons '* #'*)
                 (cons '/ #'/)
                 (cons '< #'<)
                 (cons '> #'>)))))
    (handler-case
        (loop
          (format t "~&PLOS-EVAL> ")
          (prin1 (eval (read) env)))
      (end-of-file () (format t "~%Moriturus te saluto.")))))
