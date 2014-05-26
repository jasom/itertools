;;;; itertools.lisp

(in-package #:itertools)

;;; "itertools" goes here. Hacks and glory await!

(define-symbol-macro expanders *expanders*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *expanders* nil))

(defmacro macro-bind (ll form env &body b)
  (with-gensyms (formg envg)
    (multiple-value-bind
	(new-ll wholevar envvar)
      (loop with l = ll
	 with wholevar = nil
	 with envvar = nil
	 for continue = l
	 for item = (and l (pop l))
	 while continue
	 when (eql item 'cl:&environment)
	   do (setf envvar (pop l))
	 else when (eql item 'cl:&whole)
	   do (setf wholevar (pop l))
	 else collect item into newll
	 finally (return (values newll wholevar envvar)))
    `(let ((,formg ,form)
	   (,envg ,env))
       (declare (ignorable ,envg))
	 (let
	 (,@(when envvar `((,envvar ,envg)))
	  ,@(when wholevar `((,wholevar ,formg))))
       (destructuring-bind ,new-ll (cdr ,formg)
	 ,@b))))))

(defmacro define-iterator-expander (name args &body b)
  (with-gensyms (form env)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (getf *expanders* ',name)
	   (lambda (,form ,env)
	     (declare (ignorable ,env))
	     (macro-bind ,args ,form ,env ,@b))))))

(defmacro anonymous-iterator-expander (args &body b)
  (with-gensyms (form env)
    `(lambda (,form ,env)
      (declare (ignorable ,env))
      (macro-bind ,args ,form ,env ,@b))))


(defun simple-expansion (form)
  (let ((tmp (gensym)))
    (values nil               ;temps
	    nil               ;values
	    (list tmp)        ;stores
	    `(setf ,tmp ,form)   ;initial
	    `(setf ,tmp 'eoi))));next

(defun get-iterator-expansion (form &optional (env (cons nil *expanders*)))
  (cond
    ((symbolp form)
     (simple-expansion form))
    ((listp form)
     (multiple-value-bind (expansion expanded)
	 (macroexpand-1 form (car env))
       (cond
	 (expanded
	   (get-iterator-expansion expansion env))
	 ((symbolp (car form))
	  (let ((exp (getf (cdr env) (car form))))
	    (if exp (funcall exp form env)
		(simple-expansion form))))
	 (t (simple-expansion form)))))
    (t (simple-expansion form))))


(defmacro loop-iterator (form &environment env args &body b)
  (let ((expanders expanders))
  (multiple-value-bind (tmps values stores init next cleanup)
      (get-iterator-expansion form (cons env expanders))
    `(let*
	 (,@(loop for tmp in tmps
		for value in values
		collect (list tmp value))
	  ,@stores)
       (multiple-value-setq ,stores
	 ,init)
       (unwind-protect 
	    (loop
	       for ,args = (list ,@stores)
					;do (print (list ,@stores))
	       while (not (eql ,(car stores) 'eoi))
		 ,@b
	       do (multiple-value-setq ,stores ,next))
	 ,@cleanup)))))

(defmacro do-iterator ((vars form &optional result) &body b)
  `(loop-iterator ,form ,(ensure-list vars) do (progn ,@b)
	finally (return ,result)))
       
(define-iterator-expander list (&rest lists)
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

(define-iterator-expander repeat (&environment env iter &optional (count 2))
  (multiple-value-bind
	(tmp v s i n c)
      (get-iterator-expansion iter env)
    (with-gensyms (state countg)
      (values
     `(,@tmp  ,state ,countg)
     `(,@v 0 ,count)
     s
     i
     `(if (eql ,countg :inf)
	  (values ,@s)
	  (progn
	    (setf ,state (mod (1+ ,state) ,countg))
	    (if (= ,state 0)
		,n
		(values ,@s))))
     c))))

(define-iterator-expander compress (&environment env i1 i2)
  (multiple-value-bind
	(t1 v1 s1 i1 n1 c1)
      (get-iterator-expansion i1 env)
  (multiple-value-bind
	(t2 v2 s2 i2 n2 c2)
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
	(values ,@s1))
     `(,@c1 ,@c2)))))

(define-iterator-expander chain (&environment env &rest iters)
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
		,@(loop for i in iterinfo
		       append (loop repeat (length iterinfo) collect nil)))
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
	(values ,@results))
     (loop for i in iterinfo
	      append (sixth i)))))

(define-iterator-expander count (&optional (start 0) (step 1))
  (with-gensyms (stepg current)
    (values
     (list stepg)
     (list step)
     (list current)
     start
     `(+ ,current ,stepg))))

(define-iterator-expander cycle (&rest lists)
  (let ((listg1 (loop repeat (length lists) collect (gensym)))
	(listg2 (loop repeat (length lists) collect (gensym)))
	(results (loop repeat (length lists) collect (gensym))))
    (values
    `(,@listg1 ,@listg2)
    `(,@lists ,@lists)
    `(,@results)
    `(values ,@(loop for i in listg1 collect `(pop ,i)))
    `(progn
      ,@(loop for i in listg1
	     for j in listg2
	   collect `(unless ,i (setf ,i ,j)))
      (values ,@(loop for i in listg1 collect `(pop ,i)))))))

(define-iterator-expander dropwhile (&environment env predicate iter)
  (multiple-value-bind
	(tmp val res init next)
      (get-iterator-expansion iter env)
    (with-gensyms (pred)
      (values
       `(,pred ,@tmp)
       `(,predicate ,@val)
       res
       `(progn
	  (multiple-value-setq ,res ,init)
	  (loop
	     until (eql ,(car res) 'eoi)
	     while (funcall ,pred ,@res)
	     do (multiple-value-setq ,res ,next))
	  (values ,@res))
       `(progn (multiple-value-setq ,res ,next)
	       (values ,@res))))))

(define-iterator-expander file (file-form &optional
					  (read-form '(read-line :the-file
								nil 'eoi)))
  (with-gensyms (file result)
    (values
     `(,file)
     `(,file-form)
     `(,result)
     (substitute file :the-file read-form)
     (substitute file :the-file read-form)
     `((close ,file)))))

(define-iterator-expander group-by* (iter keyfn &optional
					 (nested '(repeat (:group) 1))
					 (group-name :group)
					 &environment env)
  (multiple-value-bind
	(tmp v s i n c)
      (get-iterator-expansion iter env)
    (let ((stores (loop repeat (length s) collect (gensym)))
	    (stores2 (loop repeat (length s) collect (gensym))))
      (with-gensyms (curval kf done  local-tmp)
	(let* ((expander
	       (anonymous-iterator-expander ()
		 (values
		  nil
		  nil
		  stores2
		  `(progn
		     (if (eql ,(car s) 'eoi)
			 'eoi
			 (progn
			   (setf ,curval (funcall ,kf ,@s))
			   (values ,@s))))
		  `(progn
		     (multiple-value-setq ,s ,n)
		     (if (eql ,(car s) 'eoi)
			 'eoi
			 (let ((,local-tmp (funcall ,kf ,@s)))
			   (cond
			     ((eql ,local-tmp ,curval)
			      (setf ,done nil)
			      (values ,@s))
			     (t
			      (setf ,done t)
			      (setf ,curval ,local-tmp)
			      'eoi)))))
		  nil)))
	       (expanders (append (list group-name expander) (cdr env))))
	  (multiple-value-bind
		(nt nv ns ni nn nc)
	      (get-iterator-expansion nested
				      (cons (car env) expanders))
	    (values
	     `(,curval ,done ,kf ,@tmp ,@nt ,@ns ,@s)
	     `(nil ,nil ,keyfn ,@v ,@(make-list (+
						 (length nt)
						 (length s)
						 (length ns))))
	     stores
	     `(unwind-protect
		   (progn
		     (multiple-value-setq ,s ,i)
		     (multiple-value-setq ,nt (values ,@nv))
		     (multiple-value-setq ,ns ,ni)
		     (loop
			until (eql ,(car ns) 'eoi)
			  ,@(loop for storei in stores
			       for  si in ns
			       append `(collect ,si into ,storei))
			do (multiple-value-setq ,ns ,nn)
			finally (return
				  (if (null ,(car stores))
				      'eoi
				      (values ,@stores)))))
		,@nc)
	 `(unwind-protect
	       (progn
		   (loop until ,done
			do (multiple-value-setq ,s ,n)
			(if (eql ,(car s) 'eoi)
			    (progn
			      (setf ,done t)
			      'eoi)
			    (let ((,local-tmp (funcall ,kf ,@s)))
			      (cond
				((eql ,local-tmp ,curval)
				 (setf ,done nil)
				 (values ,@s))
				(t
				 (setf ,done t)
				 (setf ,curval ,local-tmp)
				 'eoi)))))
		 (multiple-value-setq ,nt (values ,@nv))
		 (multiple-value-setq ,ns ,ni)
		 (loop
		    until (eql ,(car ns) 'eoi)
		      ,@(loop for storei in stores
			   for  si in ns
			   append `(collect ,si into ,storei))
		    do (multiple-value-setq ,ns ,nn)
		    finally (return
			      (if (null ,(car stores))
				  'eoi
				  (values ,@stores)))))
	    ,@nc)
	 c)))))))


(define-iterator-expander group-by (iter keyfn &optional
					 &environment env)
  (multiple-value-bind
	(tmp v s i n c)
      (get-iterator-expansion iter env)
    (let ((stores (loop repeat (length s) collect (gensym))))
      (with-gensyms (curval kf)
	(values
	 `(,curval ,kf ,@tmp ,@s)
	 `(nil ,keyfn ,@v ,@(make-list (length s)))
	 stores
	 `(progn
	    (multiple-value-setq ,s ,i)
	    (setf ,curval (funcall ,kf ,@s))
	    (loop
	       until (eql ,(car s) 'eoi)
		 ,@(loop for storei in stores
		      for  si in s
		      append `(collect ,si into ,storei))
	       do (multiple-value-setq ,s ,n)
	       while (eql ,curval (funcall ,kf ,@s))
	       finally (return
			 (if (null ,(car stores))
			     'eoi
			     (values ,@stores)))))
	 `(progn
	    (setf ,curval (funcall ,kf ,@s))
	    (loop
	       until (eql ,(car s) 'eoi)
		 ,@(loop for storei in stores
		      for  si in s
		      append `(collect ,si into ,storei))
	       do (multiple-value-setq ,s ,n)
	       while (eql ,curval (funcall ,kf ,@s))
	       finally (return
			 (if (null ,(car stores))
			     'eoi
			     (values ,@stores)))))
	 c)))))

(define-iterator-expander take (iterator &optional (count 1) &environment env)
  (with-gensyms (ng)
    (multiple-value-bind
	  (tmp v s i n c)
	(get-iterator-expansion iterator env)
    (values
     `(,ng ,@tmp)
     `(,count ,@v)
     s
     `(if (> ,ng 0)
	   (progn
	     (decf ,ng)
	     ,i)
	   (values 'eoi))
     `(if (> ,ng 0)
       (progn
	 (decf ,ng)
	 ,n)
       'eoi)
     c))))

(define-iterator-expander values (&environment env &rest iters)
  (let* ((iterinfo
	  (loop for i in iters
	     collect (multiple-value-list (get-iterator-expansion i env))))
	 (results (loop repeat (length iters) 
		     collect (gensym))))
    (values
     `(
       ,@(loop for i in iterinfo append (first i))
       ,@(loop for i in iterinfo append (third i))
       )
     `(,@(loop for i in iterinfo append (second i))
	 ,@(loop for i in iterinfo
	      append (loop repeat (length (third i)) collect nil)))
     results
     `(progn
	,@(loop for i in iterinfo collect
	       `(multiple-value-setq ,(third i) ,(fourth i)))
	(if
	 (and ,@(loop for i in iterinfo collect `(not (eql ,(car (third i)) 'eoi))))
	 (values ,@(loop for i in iterinfo collect (car (third i))))
	 'eoi))
     `(progn
	,@(loop for i in iterinfo collect
	       `(multiple-value-setq ,(third i) ,(fifth i)))
	(if
	 (and ,@(loop for i in iterinfo collect `(not (eql ,(car (third i)) 'eoi))))
	 (values ,@(loop for i in iterinfo collect (car (third i))))
	 'eoi))
     (loop for i in iterinfo
	append (sixth i)))))
