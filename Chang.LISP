; listener -->(let ((*STANDARD-INPUT* (open "C:/Users/Eunji/Downloads/1800_Program1_Test_2017/test1.txt"))) (family))
; Family Tree Project: Lisp Edition
;====================================
;Jamie Chang, Ryan Scoopo, Nick Savino
;;;;-*- Mode: Commom-Lisp -*-

(defparameter tree (make-hash-table :test #'string=))

(defstruct person
	(name nil) 
	(parents nil))
                                     
;Nick

(defun getparents (p)
  (person-parents (gethash p tree))) 

(defun sibling (p q)
  (if (or (and (STRING= (first (getparents p)) (first (getparents q))) (STRING= (second (getparents p)) (second (getparents q))))
          (and (STRING= (first (getparents p)) (second (getparents q))) (STRING= (second (getparents p)) (first (getparents q)))))
      t
      nil))

(defun half-sibling (p q)
  (if (or (and (STRING= (first (getparents p)) (first (getparents q))) (not (STRING= (second (getparents p)) (second (getparents q)))))
          (and (STRING= (first (getparents p)) (second (getparents q))) (not (STRING= (second (getparents p)) (first (getparents q))))))
      t
      nil))


(defun ancestors (p) 
   (if (null (getparents p))
        nil
         (when (getparents p)       
           (append (getparents p)
                   (ancestors (first (getparents p)))
                   (ancestors (second (getparents p)))))))
 
(defun isAncestor (p1 p2)
  (let ((l2 (ancestors p2)))
    (find p1 l2 :test #'string=)))
    
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
	  (when (string= n p2) 
	     (setf bool2 t)))
       (dolist (n2 l2)
	  (when (string= n2 p1) 
	     (setf bool2 t)))
      (if (and (not (not bool1)) 
	    (not bool2))
          t
          nil)))



(defun related (p1 p2)
  (let ((l1 (ancestors p1))
        (l2 (ancestors p2))
        (bool1 nil) (bool2 nil)) 
    (dolist (name1 l1)
      (dolist (name2 l2)
        (when (string= name1 name2) 
          (setf bool1 t))))
    (dolist (n l1)
      (when (string= n p2) 
        (setf bool2 t)))
    (dolist (n2 l2)
      (when (string= n2 p1) 
        (setf bool2 t)))
    (or (or (not (not bool1)) 
	    (not (not bool2)))
        (and (not (not bool1)) (not (not bool2))))))

(defun family ()
  (let ((line nil) (words nil) (p1 nil) (p2 nil) (p3 nil) (lambsauce nil) (temp 0)) 
     ;check parenthesis
    (loop
     (setf line (read-line *standard-input* nil nil))
     (when (null line) (return))
     (setf words (SPLIT-SEQUENCE " " line))
     (cond ((STRING= (FIRST words) "E")
            (when (= (length words) 4)
              (setf p1 (gethash (SECOND words) tree))
              (setf p2 (gethash (THIRD words) tree))
            
              (when (null (gethash (SECOND words) tree))
                (setf p1 (make-person :name (SECOND words)))
                (setf (gethash (SECOND words) tree) p1))
            
              (when (null (gethash (THIRD words) tree))
                (setf p2 (make-person :name (THIRD words)))
                (setf (gethash (THIRD words) tree) p2))
            
              (setf p3 (make-person :name (FOURTH words)))
              (setf (gethash (FOURTH words) tree) p3)
              (setf (person-parents p3) (list (SECOND words) (THIRD words)))))
           
           
           ((STRING= (FIRST words) "X") ;if not in tree, and method not unrelated, say no
            (setf temp 0)
            (maphash #'(lambda (k v)(push k lambsauce)) tree)
            (setf lambsauce (remove-duplicates lambsauce :test #'equal)) ;some insurance
            (when (find (SECOND words) lambsauce :test #'string=)
              (incf temp))
            (when (find (FOURTH words) lambsauce :test #'string=)
              (incf temp))
            (when (< temp 2)
              (if (STRING= (THIRD words) "unrelated")
                   (FORMAT t "~A~%" 'Yes)  (FORMAT t "~A~%" 'No )))
            (when (= temp 2)

              (cond ((STRING= (THIRD words) "parent")
                     (let ((parents nil))
                           (setf parents (getparents (FOURTH words)))
                           (dolist (p parents)
                             (if (STRING= p (SECOND words)) 
                                     (FORMAT t "~A~%" 'Yes)  (FORMAT t "~A~%" 'No)))))
                    
                    ((STRING= (THIRD words) "sibling")
                     (if (sibling (SECOND words) (FOURTH words)) 
                         (FORMAT t "~A~%" 'Yes)  (FORMAT t "~A~%" 'No)))
                    
                    ((STRING= (THIRD words) "half-sibling")
                     (if (half-sibling (SECOND words) (FOURTH words)) 
                         (FORMAT t "~A~%" 'Yes)  (FORMAT t "~A~%" 'No)))
                    
                    ((STRING= (THIRD words) "ancestor")
                     (if (isAncestor (SECOND words) (FOURTH words))
                          (FORMAT t "~A~%" 'Yes)  (FORMAT t "~A~%" 'No)))

                    
                    ((STRING= (THIRD words) "cousin")
                     (if (cousin (SECOND words) (FOURTH words))
                        (FORMAT t "~A~%" 'No)  (FORMAT t "~A~%" 'Yes) ))
                    
                    ((STRING= (THIRD words) "unrelated")
                     (if (not (related (SECOND words) (FOURTH words)))
                         (FORMAT t "~A~%" 'Yes)  (FORMAT t "~A~%" 'No))))))   
           
           
           ((STRING= (FIRST words) "W")
            (maphash #'(lambda (k v)(push k lambsauce)) tree)
            (setf lambsauce (remove-duplicates lambsauce :test #'equal)) ;some more insurance
            (when (= (length words) 3)

              (cond ((STRING= (SECOND words) "parent")
                     (FORMAT t "~A~%" (sort (getparents (THIRD words)) #'string-lessp)))

                    ((STRING= (SECOND words) "sibling")
                     (let ((sib nil))                   
                      (loop for g in lambsauce
                            do (when (sibling g (THIRD words))
                                 (if (not(STRING= g (THIRD words)))
                                   (push g sib))))
                      (sort sib #'string-lessp)
                      (dolist (g sib)	
                        (FORMAT t "~A~%" g))))
                     
                    ((STRING= (SECOND words) "half-sibling")
                     (let ((halfies nil))
                       (loop for g in lambsauce
                             do (when (half-sibling g (THIRD words))
                                  (push g halfies)))
                       (sort halfies #'string-lessp)
                       (dolist (g halfies)	
                         (FORMAT t "~A~%" g))))
                    
                    ((STRING= (SECOND words) "ancestor")
                     (let ((alist nil))
                       (setf alist (ancestors (THIRD words)))
                       (sort alist #'string-lessp)
                       (dolist (p alist)
                         (FORMAT t "~A~%" p))))
                    
                    ((STRING= (SECOND words) "cousin")
                     (let ((cousins nil))
                       	(loop for g in lambsauce
                              do (when (cousin g (THIRD words))
                                   (push g cousins)))
                        (sort cousins #'string-lessp)
                        (dolist (g cousins)	
                          (FORMAT t "~A~%" g))))
                    
                    ((STRING= (SECOND words) "unrelated")
                     (let ((unrelatedss nil))                 
                       (loop for g in lambsauce
                             do (when (not (related g (THIRD words)))
                                 (push g unrelatedss)))
                       (sort unrelatedss #'string-lessp)
                       (dolist (g unrelatedss)	
                         (FORMAT t "~A~%" g)))))))))))
