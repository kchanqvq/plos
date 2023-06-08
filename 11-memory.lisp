(defpackage #:plos
  (:use #:cl)
  (:shadow #:eval #:apply
           #:cons #:list #:list* #:mapcar #:reverse
           #:copy-tree))
(in-package #:plos)

(defun host-gc-warning ()
  (format t "Host GC triggered. Our heap might be compromised.~%"))

(pushnew 'host-gc-warning sb-ext:*after-gc-hooks*)

;;; Memory Management

(defvar *from-space*
  (sb-posix:mmap nil (* 1024 1024) (logior sb-posix:prot-read sb-posix:prot-write)
                 (logior sb-posix:map-anon sb-posix:map-private) -1 0))

(defvar *to-space*
  (sb-posix:mmap nil (* 1024 1024) (logior sb-posix:prot-read sb-posix:prot-write)
                 (logior sb-posix:map-anon sb-posix:map-private) -1 0))

(defvar *free-addr* (sb-sys:sap-int *to-space*))
(defvar *top-addr* (+ *free-addr* (* 1024 1024)))

(defun addr-to-cons (addr)
  (sb-kernel:%make-lisp-obj (logior addr sb-vm:list-pointer-lowtag)))

(defun cons-to-addr (cons)
  (logxor (sb-kernel:get-lisp-obj-address cons) sb-vm:list-pointer-lowtag))

(defun cons (a b)
  (let ((cell (addr-to-cons *free-addr*)))
    (setf *free-addr* (+ *free-addr* 16))
    (setf (car cell) a)
    (setf (cdr cell) b)
    cell))

;; The GC ritual:
;; (flip-space)
;; (setf <some-register> (move <some-register>))
;; (scan (sb-sys:sap-int *to-space*))

(defun flip-space ()
  (rotatef *to-space* *from-space*)
  (setf *free-addr* (sb-sys:sap-int *to-space*))
  (setf *top-addr* (+ *free-addr* (* 1024 1024)))
  nil)

(defun in-space? (cell space)
  (and (consp cell)
       (let ((addr (cons-to-addr cell)))
         (and (>= addr (sb-sys:sap-int space))
              (< addr (+ (sb-sys:sap-int space) (* 1024 1024)))))))

(defun scan (scan-addr)
  (if (< scan-addr *free-addr*)
      (let ((cell (addr-to-cons scan-addr)))
        (setf (car cell) (move (car cell)))
        (setf (cdr cell) (move (cdr cell)))
        (scan (+ scan-addr 16)))
      nil))

(defun move (cell)
  (if (in-space? cell *from-space*)
      (if (eq (car cell) '%broken-heart)
          (cdr cell)
          (let ((new-cell (cons (car cell) (cdr cell))))
            (setf (car cell) '%broken-heart)
            (setf (cdr cell) new-cell)
            new-cell))
      cell))

;; Replacement for some library functions

(defun list (&rest args)
  (if args
      (cons (car args) (cl:apply #'list (cdr args)))
      nil))

(defun list* (&rest args)
  (if (cdr args)
      (cons (car args) (cl:apply #'list* (cdr args)))
      (car args)))

(defun reverse (list)
  (let (result)
    (dolist (element list)
      (setf result (cons element result)))
    result))

(defun mapcar (f list-a list-b)
  (let (result)
    (loop for a in list-a
          for b in list-b
          do (setf result (cons (cl:funcall f a b) result)))
    (reverse result)))

(defun copy-tree (tree)
  "Recursively copy the cons TREE.
Useful for copying cons trees returned by host CL library functions
into our own heap. Currently used for READ and UIOP:READ-FILE-FORMS."
  (if (consp tree)
      (cons (copy-tree (car tree))
            (copy-tree (cdr tree)))
      tree))

;;; The Interpreter

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

(defvar *pending-interrupt* nil)

(defvar *enable-interrupt-flag* nil)

(defvar *interrupt-handler*
  (lambda (interrupt)
    (error "Received ~A, but *INTERRUPT-HANDLER* is not set." interrupt)))

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
     (eval-multiple (copy-tree (uiop:read-file-forms (cadr form))) env cont))
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

(defun %current-time ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun apply (op args cont)
  (if (< (+ *free-addr* 1024) *top-addr*)
      nil
      (sb-sys:without-gcing
        (format t "Garbage Collection triggered.~%")
        (flip-space)
        (setf op (move op))
        (setf args (move args))
        (setf cont (move cont))
        (setf *interrupt-handler* (move *interrupt-handler*))
        (scan (sb-sys:sap-int *to-space*))))
  (if (and *pending-interrupt* *enable-interrupt-flag*)
      (let ((interrupt *pending-interrupt*))
        (setf *pending-interrupt* nil)
        (apply *interrupt-handler* (list interrupt nil nil)
               (cons (list* 'interrupted-apply op args) cont)))
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
        (t (error "~S is not applicable." op)))))

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
           (cl:funcall (caddr frame) value (cadr frame)))
          ((tagged-list? frame 'interrupted-apply)
           (apply (cadr frame) (cddr frame) cont))
          (t (error "Ill-formed continuation frame ~S." frame)))))

(defun boot ()
  (setf *enable-interrupt-flag* nil)
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
                 (cons 'call/cc 'call/cc)
                 (cons 'set-interrupt-handler!
                       (lambda (handler) (setf *interrupt-handler* handler)))
                 (cons 'set-timer!
                       (lambda (ticks)
                         (sb-ext:schedule-timer
                          (sb-ext:make-timer
                           (lambda () (setf *pending-interrupt* 'time)))
                          (* ticks 1e-9))))
                 (cons 'set-enable-interrupt-flag!
                       (lambda (enable?) (setf *enable-interrupt-flag* enable?)))
                 (cons 'sleep #'sleep)
                 (cons 'current-time #'%current-time)))))
    (handler-case
        (loop
          (format t "~&PLOS-EVAL> ")
          (eval (copy-tree (read)) env
                (list (list 'top-level env
                            (lambda (value new-env)
                              (prin1 value)
                              (setq env new-env))))))
      (end-of-file () (format t "~%Moriturus te saluto.")))))
