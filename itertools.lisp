;;;; itertools.lisp

(in-package #:itertools)

;;; "itertools" goes here. Hacks and glory await!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *expanders* (make-hash-table)))

(defmacro define-iterator-expander (name args &body b)
  (with-gensyms (x)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *expanders*)
	     (lambda (,x)
	       (destructuring-bind ,args ,x ,@b))))))

(defun simple-expansion (form)
  (let ((tmp (gensym)))
    (values nil               ;temps
	    nil               ;values
	    (list tmp)        ;stores
	    `(setf ,tmp ,form)   ;initial
	    `(setf ,tmp 'eoi))));next

(defun get-iterator-expansion (form &optional env)
  (cond
    ((symbolp form)
     (simple-expansion form))
    ((listp form)
     (multiple-value-bind (expansion expanded)
	 (macroexpand-1 form env)
       (cond
	 (expanded
	   (get-iterator-expansion expansion env))
	 ((and (symbolp (car form))
	       (gethash (car form) *expanders*))
	  (funcall (gethash (car form) *expanders*)
		   (cons env form))))))
    (t (simple-expansion form))))


(defmacro loop-iterator (form &environment env args &body b)
  (multiple-value-bind (tmps values stores init next)
      (get-iterator-expansion form env)
    `(let*
	 (,@(loop for tmp in tmps
		for value in values
		collect (list tmp value))
	  ,@stores)
       (multiple-value-setq ,stores
	 ,init)
       (loop
	    for ,args = (list ,@stores)
	    ;do (print (list ,@stores))
	    while (not (eql ,(car stores) 'eoi))
	    ,@b
	    do (multiple-value-setq ,stores ,next)))))

(defmacro do-iterator ((vars form &optional result) &body b)
  `(loop-iterator ,form ,(ensure-list vars) do (progn ,@b)
	finally (return ,result)))
       
(define-iterator-expander list (env name &rest lists)
  (declare (ignorable env name))
  (let ((tmps (loop repeat (length lists) collect (gensym)))
	(store (loop repeat (length lists) collect (gensym))))
  (values
   tmps
   lists
   store
   `(if (every #'identity (list ,@tmps))
	(values ,@(loop for item in tmps collect `(pop ,item)))
	(values 'eoi))
   `(if (every #'identity (list ,@tmps))
	(values ,@(loop for item in tmps collect `(pop ,item)))
	(values 'eoi)))))

(define-iterator-expander repeat (env name iter &optional (count 2))
  (declare (ignorable name))
  (multiple-value-bind
	(tmp v s i n)
      (get-iterator-expansion iter env)
    (let ((state (gensym)))
    (values
     `(,@tmp  ,state)
     `(,@v 0)
     s
     i
     `(progn
	(setf ,state (mod (1+ ,state) ,count))
	(if (= ,state 0)
	  ,n
	  (values ,@s)))))))

(define-iterator-expander compress (env name i1 i2)
  (declare (ignorable name))
  (multiple-value-bind
	(t1 v1 s1 i1 n1)
      (get-iterator-expansion i1 env)
  (multiple-value-bind
	(t2 v2 s2 i2 n2)
      (get-iterator-expansion i2 env)
    (values
     `(,@t1 ,@t2 ,@s2)
     `(,@v1 ,@v2 ,@(mapcar (lambda (x) (declare (ignore x)) nil) s2))
     `(,@s1)
     `(progn
	(multiple-value-setq ,s1 ,i1)
	(multiple-value-setq ,s2 ,i2)
	(loop until ,(car s2)
	   until (eql ,(car s1) 'eoi)
	   do
	     (multiple-value-setq  ,s1 ,n1)
	     (multiple-value-setq ,s2 ,n2))
	(values ,@s1))
     `(progn
	(multiple-value-setq  ,s1 ,n1)
	(multiple-value-setq ,s2 ,n2)
	(loop until ,(car s2)
	   until (eql ,(car s1) 'eoi)
	     do (multiple-value-setq  ,s1 ,n1)
	     do (multiple-value-setq ,s2 ,n2))
	(values ,@s1))))))

(define-iterator-expander chain (env name &rest iters)
  (declare (ignore name))
  (let* ((itertmp (gensym))
	(iterinfo
	 (loop for i in iters
	      collect (multiple-value-list (get-iterator-expansion i env))))
	(states (loop repeat (length iters) collect (gensym)))
	(results (loop repeat (loop for i in iterinfo
				   maximize (length (third i)))
		      collect (gensym)))
	(ensure-value
	 `(loop while (eql ,(car results) 'eoi)
	     do (pop ,itertmp)
	     while ,itertmp
	       do (case (car ,itertmp)
		 ,@(loop for key in states
		       for iter in iterinfo
		       collect
		       `(,key
			(multiple-value-setq ,(third iter)
			  ,(fourth iter))
			(multiple-value-setq ,results
			  (values ,@(third iter)))))))))
    (values
     `(,itertmp
       ,@(loop for i in iterinfo append (first i))
       ,@(loop for i in iterinfo append (third i))
	)
     `(',states ,@(loop for i in iterinfo append (second i))
		,@(loop repeat (length iterinfo) collect nil))
     results
     `(progn
	(push nil ,itertmp)
	(setf ,(car results) 'eoi)
	,ensure-value
	(values ,@results))
     `(progn
	(case (car ,itertmp)
	  ,@(loop for key in states
		 for i in iterinfo
		 collect
		 `(,key
		   (multiple-value-setq ,(third i) ,(fifth i))
		   (multiple-value-setq ,results (values ,@(third i))))))
	,ensure-value
	(values ,@results)))))
