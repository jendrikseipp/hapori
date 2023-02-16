;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File: btree.lisp
;;; Author: Sergio Jiménez
;;; Created: Feb 31 1 08
;;; Last modified: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure for the binary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (btree (:print-function btree-print))
  (first-node nil)
  (last-node nil)
  (pointer nil)
  (current-context nil)
  (size 0))

(defun btree-addfirst-node (btree bnode)
  (incf (btree-size btree))
  (setf (btree-first-node btree) bnode)
  (setf (btree-last-node btree) bnode)
  bnode)

(defun btree-addyes-node (btree bnode) 
  (incf (btree-size btree))
  (setf (bnode-yes-node (btree-last-node btree)) bnode)
  (setf (btree-last-node btree) bnode)
  bnode)

(defun btree-addno-node (btree bnode)
  (incf (btree-size btree))
  (setf (bnode-no-node (bnode-father (btree-last-node btree))) bnode)
  (if (bnode-test-p bnode) ;; test-node
      (setf (btree-last-node btree) bnode)
    ;; leaf-node
    (setf (btree-last-node btree) (bnode-father bnode)))
  bnode)

(defun btree-load-from-list (list-btree)
  (let ((btree nil))
    ;;creating the btree
    (setf btree (make-btree))

    ;;adding the first node
    (if (atom (nth 3 (car list-btree))) ;;; test-node
	(btree-addfirst-node btree (make-btestnode (car list-btree) nil))
      (btree-addfirst-node btree (make-bleafnode (car list-btree) nil)))
    
    ;;adding the rest of the nodes
    (dolist (item (rest list-btree))      
      (if (atom (nth 3 item))	  
	  (progn  ;;; test-node
	    (when (equal (car item) 'yes) ;;; yes-node
	      (btree-addyes-node btree (make-btestnode (rest item) (btree-last-node btree))))
	    (when (equal (car item) 'no) ;;; no-node
	      (btree-addno-node btree (make-btestnode (rest item) (bnode-father (btree-last-node btree))))))
	  (progn  ;;; leaf-node
	    (when (equal (car item) 'yes) ;;; yes-node
	      (btree-addyes-node btree (make-bleafnode item (btree-last-node btree))))
	    (when (equal (car item) 'no) ;;; no-node
	      (btree-addno-node btree (make-bleafnode item (bnode-father (btree-last-node btree))))))))
    btree))

(defun btree-load-from-tilde-file (file-path)
  (with-open-file (istream file-path :direction :input)
    (let ((btree nil)
	  (line nil))
         (loop
	   (setq line (read-line istream :eof))
	   (when (equal line "selected(-A,-B)")
	     (progn
	       (loop
		 (setq line (read-line istream :eof))
		 (when (= (length line) 0) (return))
		 (setq line (string-replace line " " ""))
		 (setq line (string-replace line "," " "))
		 (setq line (string-replace line "(" " "))
		 (setq line (string-replace line ":" " "))
		 (setq line (string-replace line "-" ""))
		 (setq line (string-replace line "?" ""))
		 (setq line (string-replace line "|" ""))
		 (setq line (string-replace line "+" ""))
		 (setq line (string-replace line "[[" "("))
		 (setq line (string-replace line "]]" "))"))
		 (setq line (string-replace line "[" ""))
		 (setq line (string-replace line "]" " "))
		 (setq line (concatenate 'string "(" line))
		 (push (read-from-string line) btree))))
	   (when btree (return)))         
	 (btree-load-from-list(reverse btree)))))

(defun btree-print (btree stream z)
  (declare (type btree btree)
	   (stream stream))
  (btree-recursive-print (btree-first-node btree) stream "" z))

(defun btree-recursive-print (bnode stream padding z)
  (setf padding (concatenate 'string padding "   "))  
  (if (bnode-test-p bnode) (progn (bnode-print bnode stream z)				  				  
				  (when (bnode-yes-node bnode)
				    (format stream "~a" padding)
				    (format stream "~a~%" (btree-recursive-print (bnode-yes-node bnode) stream padding z)))
				  (when (bnode-no-node bnode)
				    (format stream "~a" padding)
				    (format stream "~a~%" (btree-recursive-print (bnode-no-node bnode) stream padding z))))
    (progn 
      (bnode-print bnode stream z))))

(defun btree-print-tilde-format (btree stream)
  (btree-recursive-print-tilde-format (btree-first-node btree) stream ""))

(defun btree-recursive-print-tilde-format (bnode stream padding)
  (setf padding (concatenate 'string padding "   "))  
  (if (bnode-test-p bnode) (progn (bnode-print-tilde-format bnode stream)				  				  
				  (when (bnode-yes-node bnode)
				    (format stream "~a" padding)
				    (format stream " +--yes: ")
				    (format stream "~a~%" (btree-recursive-print-tilde-format (bnode-yes-node bnode) stream padding)))
				  (when (bnode-no-node bnode)
				    (format stream "~a" padding)
				    (format stream " +--no: ")
				    (format stream "~a~%" (btree-recursive-print-tilde-format (bnode-no-node bnode) stream padding))))
    (progn 
      (bnode-print-tilde-format bnode stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure for the nodes of the binary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (bnode (:print-function bnode-print))
  (type nil)
  (element nil)
  (yes-node nil)
  (no-node nil)
  (father nil))

(defun bnode-test-p (node)
   (equal (bnode-type node) :test-node))

(defun bnode-leaf-p (node)
   (equal (bnode-type node) :leaf-node))

(defun bnode-print (bnode stream z)
  (declare (type bnode bnode)
	   (stream stream))
  (format stream "~%#<BNODE> ELEMENT:")
  (if (bnode-leaf-p bnode) 
      (rleaf-print (bnode-element bnode) stream z)
    (rtest-print (bnode-element bnode) stream z)))

(defun bnode-print-tilde-format (bnode stream)
  (if (bnode-leaf-p bnode) 
      (rleaf-print-tilde-format (bnode-element bnode) stream)
    (rtest-print-tilde-format (bnode-element bnode) stream)))

(defun make-btestnode (list-testnode father)
  (make-bnode :type :test-node              
              :element (make-rtest :test (car list-testnode) :arguments (rest list-testnode))
	      :father father))

(defun make-bleafnode (list-leafnode father)
  (let ((decisions-name (remove nil (mapcar #'(lambda(x) (when (not (numberp x)) x)) (nth 3 list-leafnode))))
        (decisions-appearances (remove nil (mapcar #'(lambda(x) (when (numberp x) x)) (nth 3 list-leafnode)))))
    (make-bnode :type :leaf-node
		:element (make-rleaf :decissions (sort (mapcar #'(lambda (x y) (list x y)) 
							       decisions-name decisions-appearances)
						       #'(lambda(x y) (> (second x) (second y)))))
		:father father)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure for the element in the test nodes of the binary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (rtest (:print-function rtest-print))
  (test nil)
  (arguments nil))

(defun rtest-print (element stream z)
  (declare (type rtest element)
	   (stream stream)
	   (ignore z))
  (format stream "#<ROLLER-TEST>? ~a ~a" (rtest-test element)(rtest-arguments element)))

(defun rtest-print-tilde-format (element stream)
  (format stream "~a ?~%" (append (list (rtest-test element))(rtest-arguments element))))


;;; Structure for the elements of the leaf nodes in the binary tree
(defstruct (rleaf (:print-function rleaf-print))
  (decissions nil))

(defun rleaf-print (element stream z)
  (declare (type rleaf element)
	   (stream stream)
	   (ignore z))
  (format stream "#<ROLLER-LEAF> ~a" (rleaf-decissions element)))

(defun rleaf-print-tilde-format (element stream)
  (format stream "~a" (rleaf-decissions element)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilery functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; string replance function
(defun string-replace (str1 sub1 sub2)
  (let ((str1 (string str1))
        (str2 "")
        (sub1 (string sub1))
        (sub2 (string sub2))
        (index1 0))
    (loop
        (if (string-equal str1 sub1
                          :start1 index1
                          :end1 (min (length str1)
                                     (+ index1 (length sub1))))
            (progn
              (setq str2 (concatenate 'string str2 sub2))
              (incf index1 (length sub1)))
            (progn
              (setq str2 (concatenate 'string
                                      str2
                                      (subseq str1 index1 (1+ index1))))
              (incf index1)))
        (unless (< index1 (length str1))
          (return str2)))))

;; (setf *b2* '((CANDIDATE_TAKE_IMAGE A C D E F) (YES TARGET_GOAL_POINTING A G H) (YES CANDIDATE_TURN_TO A G H D)
;;  (YES TAKE_IMAGE 78.0 (TURN_TO 4.0 SWITCH_ON 0.0 SWITCH_OFF 0.0 CALIBRATE 1.0 TAKE_IMAGE 73.0))
;;  (NO TURN_TO 14.0 (TURN_TO 8.0 SWITCH_ON 0.0 SWITCH_OFF 0.0 CALIBRATE 1.0 TAKE_IMAGE 5.0))
;;  (NO TAKE_IMAGE 123.0 (TURN_TO 2.0 SWITCH_ON 0.0 SWITCH_OFF 0.0 CALIBRATE 1.0 TAKE_IMAGE 120.0))
;;  (NO CANDIDATE_CALIBRATE A I J K) (YES CANDIDATE_SWITCH_ON A L M)
;;  (YES SWITCH_ON 8.0 (TURN_TO 3.0 SWITCH_ON 5.0 SWITCH_OFF 0.0 CALIBRATE 0.0 TAKE_IMAGE 0.0))
;;  (NO CALIBRATE 52.0 (TURN_TO 9.0 SWITCH_ON 0.0 SWITCH_OFF 0.0 CALIBRATE 43.0 TAKE_IMAGE 0.0)) (NO CANDIDATE_SWITCH_ON A N O)
;;  (YES SWITCH_ON 60.0 (TURN_TO 18.0 SWITCH_ON 42.0 SWITCH_OFF 0.0 CALIBRATE 0.0 TAKE_IMAGE 0.0))
;;  (NO TURN_TO 231.0 (TURN_TO 229.0 SWITCH_ON 0.0 SWITCH_OFF 2.0 CALIBRATE 0.0 TAKE_IMAGE 0.0))))