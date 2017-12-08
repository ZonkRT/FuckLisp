;Jamie
;;;;-*- Mode: Commom-Lisp -*-

(defparameter tree (make-hash-table))

(defun family() 
  (setf line (read-line *standard-input*)))

(defun split-str (string) ;split-sequence (check handout)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defstruct person
	(name nil) 
	(parents nil :type list))

(write "before loop")
(let ((*STANDARD-INPUT* (open "testfile.txt"))) (family)) ;in listner not editor 'conversation w/lisp'
(write "read file");no writes at top of function
  (when in
    (write "before reading lines")
    (loop for line = (read-line in nil)
          while line do (
                         (write "inside loop")
                         (defparamater words (split-str line))
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
                               (print p3)
))))))
;jamie end

;Nick

(defun sibling (p q)
  (when (and (subsetp (person-parents p) (person-parents q) :test #'string=)
	     (subsetp (person-parents q) (person-parents p)); does subsetp take optional argument for equality
    (append(person-name q)))); what is this doing, need to fgure out
	;if testing two people are siblings, only need 'and'
	;

(defun half-sibling (p q)
  (when (or (and (member (first (person-parents p)) (person-parents q))
		 (not (member (second (person-parents p)) (person-parents q))))
	    (and (member (second (person-parents p)) (person-parents q))
		 (not (member (first (person-parents p)) (person-parents q))))))
	    
		     (first (person-parents q)))
	    (=(second(person-parents p))(second(person-parents q)))))
    (append(person-name q))))


(defun ancestors (p)
  (when (person-parents p)
    (remove-duplicates (append (person-parents p)
			       (ancestors (first (person-parents p)))
			       (ancestors (second (person-parents p))))))
	:test #'STRING=)))
     
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

;End of Ryan

;NO PARENTHESIS WITH NEW FUNCTION
;single quotes (') form lists with input instead of evaluating functions by name 
;read handout
;
