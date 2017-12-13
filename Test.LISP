; listener -->(let ((*STANDARD-INPUT* (open "C:/Users/Eunji/Downloads/1800_Program1_Test_2017/test1.txt"))) (family))

;;;;-*- Mode: Commom-Lisp -*-

(defparameter tree (make-hash-table))

(defstruct person
	(name nil) 
	(parents nil))
                                     
;Nick

(defun getparents (p)
  (person-parents p)) 

(defun sibling (p q)
  (if (or (and (subsetp (person-parents p) (person-parents q) (subsetp (person-parents q) (person-parents p)))))
      t
      nil))

(defun half-sibling (p q)
  (if (or (and (member (first (person-parents p)) (person-parents q))
		 (not (member (second (person-parents p)) (person-parents q))))
	    (and (member (second (person-parents p)) (person-parents q))
		 (not (member (first (person-parents p)) (person-parents q)))))
      t
      nil))


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
	  (when (string= n (person-name p2)) 
	     (setf bool2 t)))
       (dolist (n2 l2)
	  (when (string= n2 (person-name p1)) 
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
			(when (string= n (person-name p2)) 
				(setf bool2 t)))
       (dolist (n2 l2)
			(when (string= n2 (person-name p1)) 
				(setf bool2 t)))
       (or (or (not (not bool1)) 
	    (not (not bool2)))
			(and (not (not bool1)) (not (not bool2))))))

(defun family ()
  (let ((line nil) (words nil) (p1 nil) (p2 nil) (p3 nil)) 
     ;check parenthesis
    (loop
     (setf line (read-line *standard-input*))
     (when (null line) (return))
     (setf words (SPLIT-SEQUENCE " " line))
     (cond ((STRING= (FIRST words) "E")
            (when (= (length words) 4)
              (when (equal (gethash (SECOND words) tree) nil)
                (setf p1 (make-person :name (SECOND words)))
                (setf (gethash (SECOND words) tree) p1))

              (when (equal (gethash (THIRD words) tree) nil)
                (setf p2 (make-person :name (THIRD words)))
                (setf (gethash (THIRD words) tree) p2))

              (when (equal (gethash (FOURTH words) tree) nil)
                (setf p3 (make-person :name (FOURTH words)
                                      :parents (list (gethash (SECOND words) tree) (gethash (THIRD words) tree))))
                (setf (gethash (FOURTH words) tree) p3))))
           
           
           
           ((STRING= (FIRST words) "X") ;if not in tree, and method not unrelated, say no
            (when (= (length words) 4)
              (cond ((STRING= (THIRD words) "parent")
                     (let ((parents nil))
                           (setf parents (getparents (FOURTH words)))
                           (if (member (gethash (SECOND words) tree) parents) 
                                    (write 'Yes) (write 'No))))
                    
                    ((STRING= (THIRD words) "sibling")
                     (if (sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
                         (write 'Yes) (write 'No)))
                    
                    ((STRING= (THIRD words) "half-sibling")
                     (if (half-sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
                         (write 'Yes) (write 'No)))
                    
                    ((STRING= (THIRD words) "ancestor")
                     (let ((ancList nil))
                       (setf ancList (ancestors(gethash (FOURTH words) tree)))
                       (if (member (gethash (SECOND words) tree) ancList)
                           (write'Yes) (write 'No))))
                    
                    ((STRING= (THIRD words) "cousin")
                     (if (not (not (cousin (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
                         (write 'Yes) (write 'No)))
                    
                    ((STRING= (THIRD words) "unrelated")
                     (if (not (not (related (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
                         (write 'No) (write 'Yes))))))   
           
           
           ((STRING= (FIRST words) "W") 
            (write "in w")
            (when (= (length words) 3)
              (cond ((STRING= (SECOND words) "parent")
                     (write "in parent")
                     (let ((parents nil))
                       (setf parents (getparents (THIRD words)))
                       (write parents)
                       (sort parents #'string-lessp)
                       (dolist (p parents)	
                         (FORMAT t "~A~%" (person-name p)))))
                    
                    ((STRING= (SECOND words) "sibling")
                     (let ((sib nil))                     ;inquiry
                       (dolist (p tree)	
                         (when (sibling p (THIRD words))
                           (append sib (list (person-name p)))))
                       (sort sib #'string-lessp)
                       (dolist (p sib)	
                         (FORMAT t "~A~%" p))))
                     
                    ((STRING= (SECOND words) "half-sibling")
                     (let ((halfies nil))
                       (dolist (p tree)	
                         (when (half-sibling p (THIRD words))
                           (append halfies (list (person-name p)))))
                       (sort halfies #'string-lessp)
                       (dolist (p halfies)	
                         (FORMAT t "~A~%" p))))
                    
                    ((STRING= (SECOND words) "ancestor")
                     (let ((alist nil))
                       (setf alist (ancestors (gethash (THIRD words) tree)))
                       (sort alist #'string-lessp)
                       (dolist (p alist)
                         (FORMAT t "~A~%" (person-name p)))))
                    
                    ((STRING= (SECOND words) "cousin")
                     (let ((cousins nil))                       ;inquiry
                       (dolist (p tree)	
                         (when (cousin p (THIRD words))
                           (append cousins (list (person-name p)))))
                       (sort cousins #'string-lessp)
                       (dolist (p cousins)	
                         (FORMAT t "~A~%" p))))
                    
                    ((STRING= (SECOND words) "unrelated")
                     (let ((unrelatedss nil))                  ;inquiry
                       (dolist (p tree)	
                         (when (related p (THIRD words))
                           (append unrelatedss (list (person-name p)))))
                       (sort unrelatedss #'string-lessp)
                       (dolist (p unrelatedss)	
                         (FORMAT t "~A~%" p)))))))))))
