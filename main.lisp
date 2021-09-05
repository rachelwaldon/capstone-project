(defun to-cnf (tree parent grandparent parent-annot)

  (if (and (listp tree) (= (length tree) 1)) (to-cnf (car tree) parent grandparent parent-annot))

  ; base case
  (if (or (atom tree) (null (cdr tree))) (return-from to-cnf tree))

  ; if subtree has more than 2 children, then binarize the tree
  (cond ((> (length (cdr tree)) 2)
          (progn
            (setq sibling "")
            (cond ((listp (car(cdr tree))) (setf sibling (car(car(cdr tree)))))
                  (t (setf sibling (car(cdr tree)))))
            (cond ((= parent-annot 1)
                    (setf (cddr tree) (list (cons (concatenate `string parent "|" sibling "^" grandparent) (cddr tree)))))
                  (t (setf (cddr tree) (list (cons (concatenate `string parent "|" sibling) (cddr tree))))))
            (to-cnf (cdr tree) parent grandparent parent-annot)
            (to-cnf (car tree) (car tree) parent parent-annot)
            (if (and (not (find #\| (car tree))) (= parent-annot 1))
              (setf (car tree) (concatenate `string (car tree) "^" grandparent)))))

        ; case list of 2 lists
        ((and (listp (car tree)) (listp (cdr tree)) (= (length tree) 2))
           (progn
             ; case: second sublist was binarized
             (cond ((find #\| (caar(cdr tree)))
                      (progn
                        (to-cnf (car tree) (caar tree) parent parent-annot)
                        (to-cnf (cdr tree) parent grandparent parent-annot)))

                   ; else both sublists start with atoms
                   (t
                     (progn
                       (to-cnf (car tree) (caar tree) parent parent-annot)
                       (to-cnf (cdr tree) (caar(cdr tree)) parent parent-annot))))))

        ; else ( atom list ) OR ( atom atom ) form
        (t
          (cond ((find #\| (car tree))
                  (progn
                    (to-cnf (cdr tree) parent grandparent parent-annot)
                    (to-cnf (car tree) parent grandparent parent-annot)))
                (t
                  (progn
                    (to-cnf (cdr tree) (car tree) parent parent-annot)
                    (to-cnf (car tree) (car tree) parent parent-annot)
                    (if (= parent-annot 1)
                      (setf (car tree) (concatenate `string (car tree) "^" grandparent))))))))
  tree
)

(defun unary-collapse (tree)
  
  (cond
    ; base case
    ((null tree) nil)

    ; subtree is an atom
    ((atom tree) tree)

    ; subtree has one child
    ((and (atom (car tree)) (listp (car(cdr tree))) (= (length (cdr tree)) 1))
       (cons (format nil "~a+~a" (car tree) (caadr tree)) (mapcar #'unary-collapse (cdadr tree)) ))

    ; first element is atom
    ((atom (car tree))
         (cons (car tree) (mapcar #'unary-collapse (cdr tree))))

    ; else
    (t  (cons (unary-collapse (car tree)) (unary-collapse (cdr tree)))))
)

(defun count-symbols (tree sym-count words-count unary-count binary-count)

  (cond ((null tree) ())
       ; tree is atom
       ((atom tree)
          (incf (gethash tree sym-count 0)))
        ; (non-terminal word)
        ((and (atom (car tree)) (atom (cadr tree)))
           (progn
             (incf (gethash (car tree) sym-count 0))
             (incf (gethash (cadr tree) words-count 0))
             (incf (gethash tree unary-count 0))))
        ; two non-terminal children
        ((and (atom (car tree)) (= 2 (length (cdr tree))))
           (progn
             (incf (gethash (list (car tree) (caadr tree) (caaddr tree)) binary-count 0))
             (count-symbols (car tree) sym-count words-count unary-count binary-count)
             (count-symbols (cdr tree) sym-count words-count unary-count binary-count)))
        ; else recurse
        (t (progn
             (count-symbols (car tree) sym-count words-count unary-count binary-count)
             (count-symbols (cdr tree) sym-count words-count unary-count binary-count))))
)

(defun generate-grammar (sym-count words-count unary-count binary-count)

  (with-open-file (stream "grammarfile" :direction :output)

    (loop for key being the hash-keys of binary-count
      using (hash-value value)
      do (progn
           (setq lhs (car key))
           (setq prob (/ (coerce value 'double-float) (gethash lhs sym-count)))
           (format stream "~d~%" (list "P2" lhs (cadr key) (caddr key) prob))))
    (loop for key being the hash-keys of unary-count
      using (hash-value value)
      do (progn
           (setq lhs (car key))
           (setq prob (/ (coerce value 'double-float) (gethash lhs sym-count)))
           (format stream "~d~%" (list "P1" lhs (cadr key) prob))))))

(defun read-string-label (fstream)
  (loop with string-label = (make-array 0
					:element-type `character
					:fill-pointer 0
					:adjustable t)
	
	for char = (read-char fstream nil)
	while (and (char/= char #\Space) (char/= char #\() (char/= char #\Newline) (char/= char #\)))
	do (vector-push-extend char string-label)
	finally (progn
		  (when char
		    (unread-char char fstream))
		  (return string-label))))

(defun clean-label (label)
    (loop with string-label = (make-array 0
                                        :element-type `character
                                        :fill-pointer 0
                                        :adjustable t)
	for index from 0 to (-(length label) 1)
	while (char/= (aref label index) #\-)
	do (vector-push-extend (aref label index) string-label)
	finally (return string-label)))

(defun remove-NONE-parent (tree)
  (cond 
    ((null tree) nil)
    ((and (listp (car tree)) (= (length (car tree)) 1)) (remove-NONE-parent (cdr tree)))
    ((atom (car tree)) (cons (car tree) (remove-NONE-parent (cdr tree))))
    (t (cons (remove-NONE-parent (car tree)) (remove-NONE-parent (cdr tree))))))
    
; Return the tree read from file
(defun read-tree (fstream)
      
    ; skip white space
    (let ((char (read-char fstream nil)))
      (loop while (or (char= char #\Space) (char= char #\Newline)) do  
        (setq char (read-char fstream nil)))
       
	    ; branch logic based on if character is '(' ')' or letter or terminal word.
        (cond ((char= char #\()
	       	       
	     ; Remove NONE tags
	     (if  (char= (peek-char t fstream) #\-)
	       (progn
		 (setq char (read-char fstream nil))
		 (loop while (char/= char #\)) do
		   (setq char (read-char fstream nil)))
		 (read-tree fstream))

	     (progn
	        (let ((tree `()))
		  (push (clean-label (read-string-label fstream)) tree)         
		   
		; Read and append subtrees 
		(let ((subtree ()))
		(loop with is-subtree = t
		  while is-subtree do
		    (setq subtree (read-tree fstream))
		    (cond (subtree (setq tree (append tree (list subtree))))   
		          (t (setq is-subtree nil)))))
		  
		; read one character past ) after return 
		(return-from read-tree tree)))))

	   ; else if char is ')'
           ((char= char #\)) 
	      (return-from read-tree ()))
	      
	   ; else character is first letter of a word/terminal
           (t 
	     (unread-char char fstream)
	     (return-from read-tree (read-string-label fstream)))))
)

; Main function
(let ((dir (directory "/home/rwaldon/Capstone/treebank/combined/*.mrg")))
  (setq sym-count (make-hash-table :test 'equal))
  (setq words-count (make-hash-table :test 'equal))
  (setq unary-count (make-hash-table :test 'equal))
  (setq binary-count (make-hash-table :test 'equal))

  (dolist (file dir)
    (with-open-file (fstream (namestring file))
      (loop for c = (read-char fstream nil)
  	while c do
  	(read-char fstream nil)
 	(setq my-tree (remove-NONE-parent(read-tree fstream)))
	(setf my-tree (unary-collapse (to-cnf my-tree (car my-tree) "ROOT" 0)))
	(print my-tree)
	(count-symbols my-tree sym-count words-count unary-count binary-count)
	(terpri)
	(read-char fstream nil)
	(read-char fstream nil))))
  (generate-grammar sym-count words-count unary-count binary-count)  
)

