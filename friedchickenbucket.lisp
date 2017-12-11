; listener -->(let ((*STANDARD-INPUT* (open "C:/Users/Eunji/Documents/testfile.txt"))) (family))
;something is out of bound and something about in

;;;;-*- Mode: Commom-Lisp -*-

(defparameter tree (make-hash-table))

(defstruct person
	(name nil) 
	(parents nil :type list))

(defpackage :split-sequence
  (:use :common-lisp)
  (:export #:split-sequence
           #:split-sequence-if
           #:split-sequence-if-not))

(in-package :split-sequence)

(defun family() 
  (setf line (read-line *standard-input*))
  (when in
    (write "before reading lines")
    (loop for line = (read-line in nil)
        (while line do (
        (write "inside loop")
            (defparamater words (split-sequence:SPLIT-SEQUENCE #\Space line))
            (if (eq (FIRST (words)) 'e)
                (if = length (words) 4
                    (if eq (gethash (SECOND (words)) tree) nil 
                        (setf p1 (make-person :name SECOND (words)))
                        (setf (gethash (SECOND (words)) tree) p1))
                                  
                    (if eq (gethash (THIRD (words)) tree) nil
                        (setf p2 (make-person :name THIRD (words)))
                         (setf (gethash (THIRD (words)) tree) p2))

                    (if eq (gethash (FOURTH (words)) tree) nil
                        (setf p3 (make-person :name FOURTH (words)
						                      :parents (list SECOND (words) THIRD (words))))
                        (setf (gethash (FOURTH (words)) tree) p3))
                    (print p1)
                    (print p2)
                    (print p3)))
							   
			(if (eq (FIRST words) 'x)
				(if = length(words) 4 
					(when (eq (THIRD words) 'parent)
						(setf parent (gethash (SECOND words tree)))
						(setf kid (gethash (FOURTH words tree)))
							(if (or (eq (first person-parents kid) parent) (second person-parents kid) parent)
								(write 'Yes) (write 'No)))
	
					(when (eq (THIRD words) 'sibling)
						(if (sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
							(write 'Yes) (write 'No)))
		
					(when (eq (THIRD words) 'half-sibling)
						(if (half-sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
							(write 'Yes) (write 'No)))
	
					(when (eq (THIRD(words) 'ancestor)
						(setf ancList (gethash (FOURTH words) tree))
						(setf flag nil)
						(dolist (pa ancList)
						(when (eq pa (gethash (SECOND words) tree)) (setf flag t)))
							(if (not (not flag)) (write 'Yes) (write 'No))))
	
					(when (eq (THIRD words) 'cousin)
						(if (not (not (cousin (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
							(write 'Yes) (write 'No)))
	
					(when (eq (THIRD words) 'unrelated)
						(if (not (not (related (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
							(write 'No) (write 'Yes)))))   
							   
			(if (eq (FIRST (words)) 'w)
				(if = length(words) 3
					(when (eq (SECOND (word) 'parent))
					(setf parents person-parents (gethash (THIRD (words)) tree))
					(sort parents #'string-lessp)
						(dolist (p parents)	
							(write person-name p)))
							
					(when (eq (SECOND (word) 'sibling))
						(setf siblings :type :list)
						(dolist (p tree)	
							(if sibling p (SECOND (words))
								append( siblings (person-name p))))
						(sort siblings #'string-lessp)
						(dolist (p siblings)	
							(write person-name p)))
							
					(when (eq (SECOND (word) 'half-sibling))
						(setf halfies :type :list)
						(dolist (p tree)	
							(if half-sibling p (SECOND (words))
								append( halfies (person-name p))))
						(sort halfies #'string-lessp)
						(dolist (p halfies)	
							(write person-name p)))
							
					(when (eq (SECOND (word) 'ancestor))
						(setf alist ancestors ((gethash (THIRD (words)) tree)))
						(sort alist #'string-lessp)
						(dolist (p alist)
							(write person-name p)))
							
					(when (eq (SECOND (word) 'cousin))
						(setf cousins :type :list)
						(dolist (p tree)	
							(if cousin p (SECOND (words))
								append( cousins (person-name p))))
						(sort cousins #'string-lessp)
						(dolist (p cousins)	
							(write person-name p)))
								
					(when (eq (SECOND (word) 'unrelated))
						(setf unrelatedss :type :list)
						(dolist (p tree)	
							(if unrelated p (SECOND (words))
								append( unrelatedss (person-name p))))
						(sort unrelatedss #'string-lessp)
						(dolist (p unrelatedss)	
							(write person-name p)))))
)))))
                                         
                                            

                                     
;Nick

(defun sibling (p q)
  (when (and (subsetp (person-parents p) (person-parents q) :test #'string=)
	     (subsetp (person-parents q) (person-parents p)); does subsetp take optional argument for equality
    (append(person-name q))))); what is this doing, need to fgure out
	;if testing two people are siblings, only need 'and'
	;

(defun half-sibling (p q)
  (when (or (and (member (first (person-parents p)) (person-parents q))
		 (not (member (second (person-parents p)) (person-parents q))))
	    (and (member (second (person-parents p)) (person-parents q))
		 (not (member (first (person-parents p)) (person-parents q))))))
    (append(person-name q)))


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

;NO PARENTHESIS WITH NEW FUNCTION
;single quotes (') form lists with input instead of evaluating functions by name 
;read handout
;

