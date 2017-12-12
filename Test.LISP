; listener -->(let ((*STANDARD-INPUT* (open "C:/Users/Eunji/Documents/testfile.txt"))) (family))

;;;;-*- Mode: Commom-Lisp -*-

(defparameter tree (make-hash-table))

(defstruct person
	(name) 
	(parents))
                            
;Nick

(defun getparents (p)
  (temp (person-parents p))) 

(defun sibling (p q)
  (if (or (and (subsetp (person-parents p) (person-parents q) (subsetp (person-parents q) (person-parents p)))))
      (t)(nil)))

(defun half-sibling (p q)
  (if (or (and (member (first (person-parents p)) (person-parents q))
		 (not (member (second (person-parents p)) (person-parents q))))
	    (and (member (second (person-parents p)) (person-parents q))
		 (not (member (first (person-parents p)) (person-parents q)))))
    (t)(nil)))


(defun ancestors (p)
  (when (person-parents p)
    (remove-duplicates (append (person-parents p)
			       (ancestors (first (person-parents p)))
			       (ancestors (second (person-parents p))))))
  :test #'STRING=)
     
;End of Nick

;Ryan

(defun cousin (p1 p2)
  (let ((l1 (ancestors p1))
	(l2 (ancestors p2))
	(bool1 nil) (bool2 nil)) 
      (dolist (name1 l1)
	(dolist (name2 l2)
	   (when (string= name1 name2) 
		(setf bool1 t))))
       (dolist (n l1)
	  (when (string= n p2-name) 
	     (setf bool2 t)))
       (dolist (n2 l2)
	  (when (string= n2 p1-name) 
	     (setf bool2 t)))
       (and (not (not bool1)) 
	    (not bool2))))

(defun related (p1 p2)
  (let ((l1 (ancestors p1))
		(l2 (ancestors p2))
		(bool1 nil) (bool2 nil)) 
      (dolist (name1 l1)
		(dolist (name2 l2)
			(when (string= name1 name2) 
				(setf bool1 t))))
       (dolist (n l1)
			(when (string= n p2-name) 
				(setf bool2 t)))
       (dolist (n2 l2)
			(when (string= n2 p1-name) 
				(setf bool2 t)))
       (or (or (not (not bool1)) 
	    (not (not bool2)))
			(and (not (not bool1)) (not (not bool2))))))

;End of Ryan

(defun family ()
  (let ((line nil) (words)) 
    (setf line (read-line *standard-input*)) ;check parenthesis
    (loop
     (when (null line) (return)
       (setf words (SPLIT-SEQUENCE " " line))
       (cond ((STRING= (FIRST words) "E")
              (when (= (length words) 4)
                (when (= (gethash (SECOND words) tree) nil)
                  (setf p1 (make-person :name SECOND (words)))
                  (setf (gethash (SECOND words) tree) p1))
                (when (= (gethash (THIRD words) tree) nil)
                  (setf p2 (make-person :name THIRD (words)))
                  (setf (gethash (THIRD words) tree) p2))
                (when (= (gethash (FOURTH words) tree) nil)
                       (setf p3 (make-person :name FOURTH (words)))
                       (setf p3 (make-person :parents (list (gethash (SECOND words) tree) (gethash (THIRD words) tree))))
                       (setf (gethash (FOURTH words) tree) p3))))
                       
             
             
             ((STRING= (FIRST words) "X") 
              (when (= (length words) 4)
                (cond ((STRING= (THIRD words) "parent")
                       (setf parent (gethash (SECOND words tree)))
                       (setf kid (gethash (FOURTH words tree)))
                       (if (or (STRING= (first person-parents kid) parent) (second person-parents kid) parent)
                           (write 'Yes) (write 'No)))
                      
                      ((STRING= (THIRD words) "sibling")
                       (if (sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
                           (write 'Yes) (write 'No)))
                      
                      ((STRING= (THIRD words) "half-sibling")
                       (if (half-sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
                           (write 'Yes) (write 'No)))
                      
                      ((STRING= (THIRD words) "ancestor")
                       (setf ancList (gethash (FOURTH words) tree))
                       (setf flag nil)
                       (dolist (pa ancList)
                         (when (eq pa (gethash (SECOND words) tree)) (setf flag t)));what is compared here
                       (if (not (not flag)) (write 'Yes) (write 'No)))
                      
                      ((STRING= (THIRD words) "cousin")
                       (if (not (not (cousin (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
                           (write 'Yes) (write 'No)))
                      
                      ((STRING= (THIRD words) "unrelated")
                       (if (not (not (related (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
                           (write 'No) (write 'Yes))))))   
             
             
             ((STRING= (FIRST words) "W") 
              (when(= (length words) 3)
                (cond ((STRING= (SECOND words) "parent")
                       (setf parents (getparents (THIRD words)))
                       (sort parents #'string-lessp)
                       (dolist (p parents)	
                         (write person-name p)))
                      
                      ((STRING= (SECOND words) "sibling")
                       (setf sib nil type :list)                     ;inquiry
                       (dolist (p tree)	
                         (when sibling p (SECOND (words))
                           (append (sib (person-name p)))))
                       (sort sib #'string-lessp)
                       (dolist (p sib)	
                         (write p)))
                      
                      ((STRING= (SECOND words) "half-sibling")
                       (setf halfies nil type :list)                       ;inquiry
                       (dolist (p tree)	
                         (when half-sibling p (SECOND words)
                           (append( halfies (person-name p)))))
                       (sort halfies #'string-lessp)
                       (dolist (p halfies)	
                         (write p)))
                      
                      ((STRING= (SECOND words) "ancestor")
                       (setf alist (ancestors (gethash (THIRD words) tree)))
                       (sort alist #'string-lessp)
                       (dolist (p alist)
                         (write person-name p)))
                      
                      ((STRING= (SECOND words) "cousin")
                       (setf cousins nil type :list)                       ;inquiry
                       (dolist (p tree)	
                         (when cousin p (SECOND words)
                           (append (cousins (person-name p)))))
                       (sort cousins #'string-lessp)
                       (dolist (p cousins)	
                         (write p)))
                      
                      ((STRING= (SECOND words) "unrelated")
                       (setf unrelatedss nil type :list)                   ;inquiry
                       (dolist (p tree)	
                         (when related p (SECOND words)
                           (append( unrelatedss (person-name p)))))
                       (sort unrelatedss #'string-lessp)
                       (dolist (p unrelatedss)	
                         (write p))))))
             
             
             )))))
