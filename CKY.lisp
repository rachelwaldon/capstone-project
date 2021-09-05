(defstruct binaryprod
  (lhs "" :type string)
  (rhs1 "" :type string)
  (rhs2 "" :type string)
  (prob 0.0 :type double-float))

(defstruct unaryprod
  (lhs "" :type string)
  (rhs "" :type string)
  (prob 0.0 :type double-float))

; words @param array of words of the sentence
; unaryrules @param list of unary productions
; binaryrules @param list of binary productions
(defun cky-parser (words unaryrules binaryrules)
  (setq num-words (array-total-size words))
  (setq size (+ 1 (array-total-size words)))
  
  ; chart is 2d array (n+1) by (n+1)
  (setq chart (make-array (list size size) :element-type `list))
  
  ; CKY algorithm
  (loop for j from 1 to num-words
    do (progn
    ; fill in all lhs where lhs -> terminal word in grammar 
    (loop for item in unaryrules
      do (progn
           (if (string= (aref words (- j 1)) (unaryprod-rhs item))
             (push (unaryprod-lhs item) (aref chart (- j 1) j)))))))

  ; CKY algorithm
  (loop for j from 1 to num-words
   do (progn
     (if (> j 1)
       (loop for i from (- j 2) downto 0
        do (progn
          (loop for k from (+ i 1) to (- j 1)
          do (progn
            (loop for elt1 in (aref chart i k)
            do (progn
              (loop for elt2 in (aref chart k j)
                do (progn
                  (loop for prod in binaryrules
                  do (progn
                    (if (and (string= elt1 (binaryprod-rhs1 prod)) (string= elt2 (binaryprod-rhs2 prod)))
                      (progn
                        (setq lhs (cons (binaryprod-lhs prod) nil))
                        (setf (aref chart i j) (union (aref chart i j) lhs :test #'string=)))))))))))))))))
  (return-from cky-parser chart)

)

(defun split-string (str)
  (loop for i = 0 then (1+ j)
    as j = (position #\Space str :start i)
    collect (subseq str i j)
    while j))

(defun main ()
  (setq binaryrules (list))
  (setq unaryrules (list))
  (setq sentence (read-line))
  (if (find #\. sentence)
    (setq sentence (subseq sentence 0 (- (length sentence) 1))))
  (setq wordlist (split-string sentence))
  (setq words (make-array (list (length wordlist))
    :initial-contents wordlist))
  
  ; reads grammar file and stores productions
  (with-open-file (stream "grammarfile")
    (loop for line = (read-line stream nil)
      while line do
        (setf str (subseq line 1 (- (length line) 1)))
        (setf str (split-string str))
      
        ; if production is binary
        (cond ((string= "P2" (car str))
             (progn
               (setq tmp (car(cdr(cdddr str))))
               (setq p (read-from-string tmp))
               (push (make-binaryprod :lhs (cadr str) :rhs1 (caddr str) :rhs2 (cadddr str)
               :prob p) binaryrules)))
        
             ; else production is unary
             (t
               (progn
                 (setq tmp (car(cdddr str)))
                 (setq p (read-from-string tmp))
                 (push (make-unaryprod :lhs (cadr str) :rhs (caddr str) :prob p) unaryrules))))))
  
  ; call CKY algorithm
  (setq final-chart (cky-parser words unaryrules binaryrules))
  (terpri)
  (terpri)
  (dotimes (i size)
   (progn  
    (dotimes (j size)
      (print (aref final-chart i j)))
    (terpri)))
  (cond ((member "S" (aref final-chart 0 (length wordlist)) :test #'string=)
           (print "Valid sentence."))
        (t (print "Not a valid sentence.")))

); end main
 

; call main()
(main)
