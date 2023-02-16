;;; This is the file with all this Lisp functions that I use across applications

(in-package "COMMON-LISP-USER")

(setf *basic-functions-defined* t)

(eval-when (load eval compile)
  
  (defmacro secure-read (istream)
    `(handler-case (read ,istream)
       (end-of-file (c) :eof)))
  )

(defun load-and-compile (&optional (files (if (boundp '*ipss-files*) *ipss-files*))
			 &key (load-path (concatenate 'string *my-planning-path* "ipss/"))
			      (compile-path  (concatenate 'string *my-planning-path* "ipss/"
							  #+sbcl ".sbcl/"
							  #+(and clisp unix) ".clisp/"
							  #+(and clisp win32) "dosbin/"
							  #+allegro ".allegro/"))
			      (re-load-p t)
			      (load-only-source-p nil))
  (declare (special *ipss-files* *my-planning-path*))
  (ensure-directories-exist compile-path)
  (dolist (file files)
    (if re-load-p
	(load (format nil "~a~(~a~).lisp" load-path file)))
    (unless load-only-source-p
      (compile-file (format nil "~a~(~a~)" load-path file)
		    :output-file (format nil "~a~(~a~).~a" compile-path file
					 #+(or sbcl allegro) "fasl" #+clisp "fas" #+lispworks "xfasl"))
      (load (format nil "~a~(~a~)" compile-path file)))))

;; It returns a random element from list
(defun choose-one (list)
  (if list
      (nth (random (length list)) list)))

;; It returns a random element from list different from the one given as argument
(defun choose-diff-one (elem list)
  (let* ((number-elems (length list))
	 (new-elem (nth (random number-elems)
			list)))
    (if (and (> number-elems 1)
	     (equal new-elem elem))
	(choose-diff-one elem list)
      new-elem)))

(defun diff (x y)
  (not (eq x y)))

;; Comprueba si dos conjuntos son iguales
(defun equal-set-p (set1 set2 &key (test #'equal))
  (null (set-exclusive-or set1 set2 :test test)))

;; It returns the euclidean distance between two points
(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defun in-between (x x1 x2)
  (and (> x x1) (< x x2)))

;; It prints the contents of a hash table
(defun pp-hash-table (hash-table)
  (maphash #'(lambda (key val)
	       (format t "~%~a: ~a" key val))
	   hash-table))

;; It prints a list of elements in a stream with a tab
(defun pp-list (list &optional (tab 1) (ostream t) lowercasep same-line-p init-newline-p)
  (if init-newline-p (terpri ostream))
  (dolist (literal list)
      (format ostream (format nil (concatenate 'string (if lowercasep "~~~dt~~(~~s~~)" "~~~dt~~s")
					       (if same-line-p " " "~~%"))
			      tab)
	      literal)))

(defun elapsed-time (initial-time &optional (units 'run-time))
  (float (/ (- (if (eq units 'run-time)
		   (get-internal-run-time)
		   (get-internal-real-time))
	       initial-time)
	    internal-time-units-per-second 1.0)))

(defun remaining-time-p (time-limit init-time units)
  (> (- time-limit (elapsed-time init-time units)) 0))

(defun copy-array (array &optional (size (array-dimension array 0)))
  "It generates a copy of a square array"
  (let ((array1 (make-array (list size size) :initial-element 0)))
    (dotimes (i size)
	(dotimes (j size)
	    (setf (aref array1 i j)
		  (aref array i j))))
    array1))

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Given two points, it returns a list of a and b, constants that define the line equation (y=ax+b)
(defun compute-line-equation (point1 point2)
  (let* ((denominator (- (car point1) (car point2)))
	 (slope (if (zerop denominator)
		    most-positive-fixnum
		    (/ (- (cadr point1) (cadr point2))
		       denominator))))
    (list slope (if (< slope most-positive-fixnum)
		    (- (cadr point1) (* slope (car point1)))
		    (car point1)))))

;; known-var can be either 'x or 'y
(defun substitute-in-equation (equation x y known-var)
  (if (< (car equation) most-positive-fixnum)
      (if (eq known-var 'x)
	  (list x (+ (* (car equation) x) (cadr equation)))
	  (list (/ (- y (cadr equation)) (car equation)) y))
      (if (eq known-var 'y)
	  (list (cadr equation) y))))

;; non-destructive version of sort
(defun my-sort (l test)
  (sort (copy-seq l) test))

;; it returns the position of the element in a list with the minimum value of the function call and the minimum value
(defun argmin (list function)
  (let ((min-elem 0)
	(min-value (funcall function (car list)))
	(value 0)
	(i 0))
    (dolist (elem (cdr list))
      (incf i)
      (setq value (funcall function elem))
      (when (< value min-value)
	(setq min-value value)
	(setq min-elem i)))
    (values min-elem min-value)))

;; it returns the position of the element in a list with the maximum value of the function call and the maximum value
(defun argmax (list function)
  (let ((min-elem (car list))
	(min-value (funcall function (car list)))
	(value 0)
	(i 0))
    (dolist (elem (cdr list))
      (incf i)
      (setq value (funcall function elem))
      (when (> value min-value)
	(setq min-value value)
	(setq min-elem i)))
    (values min-elem min-value)))

;; generic (lisp-independent) shell execution
(defun execute-shell-command (command &optional (bash-file (concatenate 'string *my-tmp-path* "run-shell")))
  #-sbcl (run-shell-command command :wait t)
  #+sbcl (progn (with-open-file (ostream bash-file :direction :output :if-exists :supersede :if-does-not-exist :create)
 		  (format ostream "#/bin/bash~%~a~%" command))
		(sb-ext:run-program "/bin/bash" (list bash-file))))

(defun timed-execute-shell-command (command &key (bash-file (concatenate 'string *my-tmp-path* "run-shell")) (timeout nil) (time-units 'real-time))
  #-sbcl (run-shell-command command :wait t)
  #+sbcl (progn (with-open-file (ostream bash-file :direction :output :if-exists :supersede :if-does-not-exist :create)
		  (format ostream "#/bin/bash/~%~a~%" command))
		(do ((process (sb-ext:run-program "/bin/bash" (list bash-file) :wait nil))
		     (time (if (eq time-units 'real-time)
			       (get-internal-real-time)
			       (get-internal-run-time))))
		    ((or (>= (elapsed-time time time-units) timeout)
			 (not (eq (sb-ext::process-status process) :running)))
		     (sb-ext::process-kill process 9 :process-group)
		     (elapsed-time time time-units)))))

;; returns the contents of a file
;; handle-corrupted-p: it T, the file is corrupted and the file contents starts with ( it reads and returns until the
;;                     corrupted data
(defun read-all-file (file &optional handle-corrupted-p)
  (let ((what nil))
    (when (probe-file file)
      (with-open-file (input file :direction :input)
	(setq what (secure-read input)))
      (when (and (eq what :eof) handle-corrupted-p)
	(with-open-file (input file :direction :input)
	  (setf what (read-char input nil))
	  (when (or (eq what #\() (eq what #\,))
	    (setq what nil)
	    (do ((elem (secure-read input) (secure-read input)))
		((eq elem :eof) (setq what (reverse what)))
	      (push elem what))))))
    (if (not (eq what :eof))
	what)))

(defun average (sample-list &optional (sample-size (length sample-list)))
  (if sample-list
      (/ (apply #'+ sample-list) sample-size 1.0)))

(defun median (sample-list sample-size)
  (declare (ignore sample-size))
  (if sample-list
      (let* ((size (length sample-list))
	     (middle-point (floor size 2))
	     (sorted-list (sort sample-list #'<=)))
	(cond ((= size 1) (car sorted-list))
	      ((evenp size)
	       (/ (+ (nth middle-point sorted-list)
		     (nth (1- middle-point) sorted-list))
		  2.0))
	      (t (nth middle-point sorted-list))))
      0))

(defun shuffle (list &key (test #'eql))
  (if (cdr list)
      (let ((elem (choose-one list)))
	(cons elem (shuffle (remove elem list :test test) :test test)))
      list))

(defun insert-in-position (element list position)
  (append (butlast list (- (length list) position))
	  (list element)
	  (nthcdr position list)))

(defun parts-set (list)
  (if list
      (let ((parts-set-cdr (parts-set (cdr list))))
	(append (list (list (car list)))
		(mapcar #'(lambda (subparts) (cons (car list) subparts))
			parts-set-cdr)
		parts-set-cdr))))

(defun from-list-to-string (list)
  (format nil "~(~{~a~^-~}~)" list))

