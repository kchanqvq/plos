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
    ((symbolp form) (next cont (lookup env form)))
    ((tagged-list? form 'quote) (next cont (cadr form)))
    ((tagged-list? form 'if)
     (eval (cadr form) env
           (cons (list 'if-cont env (caddr form) (cadddr form)) cont)))
    ((tagged-list? form 'progn)
     (eval-multiple (cdr form) env cont))
    ((tagged-list? form 'lambda)
     (next cont (list* 'closure env (cadr form) (cddr form))))
    ((tagged-list? form 'define)
     (eval (caddr form) env
           (cons (list 'define-cont env (cadr form)) cont)))
    ((tagged-list? form 'load)
     (eval-multiple (uiop:read-file-forms (cadr form)) env cont))
    ((consp form)
     (mapeval form nil env
              (cons (list 'mapeval-complete) cont)))
    (t (next cont form))))

(defun mapeval (forms results env cont)
  (if (consp forms)
      (eval (car forms) env
            (cons (list 'mapeval-cont env (cdr forms) results) cont))
      (next cont (reverse results))))

(defun eval-multiple (forms env cont)
  "Evaluate FORMS one by one in ENV.
Return the value of the last form."
  (if (cdr forms)
      (eval (car forms) env
            (cons (list 'eval-multiple-cont env (cdr forms)) cont))
      (eval (car forms) env cont)))

(defun apply (op args cont)
  (cond
    ((functionp op) (next cont (cl:apply op args)))
    ((tagged-list? op 'closure)
     (eval-multiple (cdddr op)
                    (cons (mapcar #'cons (caddr op) args)
                          (cadr op))
                    cont))
    ((eq op 'call/cc)
     (apply (car args) (list (cons 'continuation cont)) cont))
    ((tagged-list? op 'continuation)
     (next (cdr op) (car args)))
    (t (error "~S is not applicable." op))))

(defun next (frames value)
  (let ((frame (car frames))
        (cont (cdr frames)))
    (cond ((tagged-list? frame 'if-cont)
           (if value
               (eval (caddr frame) (cadr frame) cont)
               (eval (cadddr frame) (cadr frame) cont)))
          ((tagged-list? frame 'define-cont)
           (next cont (define (cadr frame) (caddr frame) value)))
          ((tagged-list? frame 'mapeval-cont)
           (mapeval (caddr frame) (cons value (cadddr frame)) (cadr frame) cont))
          ((tagged-list? frame 'mapeval-complete)
           (apply (car value) (cdr value) cont))
          ((tagged-list? frame 'eval-multiple-cont)
           (eval-multiple (caddr frame) (cadr frame) cont))
          ((tagged-list? frame 'top-level)
           (cl:funcall (cadr frame) value))
          (t (error "Ill-formed continuation frame ~S." frame)))))

(defun boot ()
  (let ((*print-circle* t)
        (env
          (list
           (list (cons nil nil)
                 (cons t t)
                 (cons 'eq? #'eq)
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
                 (cons 'cadddr #'cadddr)
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
    #+nil (eval '(load "5-process.plos") env
                (list (list 'top-level (lambda (result) (declare (ignore result)) nil))))
    (eval '(load "5-cont-mark.plos") env
          (list (list 'top-level (lambda (result) (declare (ignore result)) nil))))
    (handler-case
        (loop
          (format t "~&PLOS-EVAL> ")
          (eval (read) env (list (list 'top-level #'prin1))))
      (end-of-file () (format t "~%Moriturus te saluto.")))))
