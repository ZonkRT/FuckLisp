;Jamie
(defparameter tree (make-hash-table))

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defstruct person
	(name nil) 
	(parents nil :type list))

(let ((in (open "C:\Users\Eunji\Downloads\testfile.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (
                         (defparamater words (split-str line))
                         (if (eq (FIRST(words)) 'e)
                             (if = length(words) 4
                               (if eq (gethash (SECOND(words)) tree) nil 
                                 (setf p1 (make-person :name SECOND(words)))
                                 (setf (gethash (SECOND(words)) tree) p1))
                                  
                               (if eq (gethash (THIRD(words)) tree) nil
                                 (setf p2 (make-person :name THIRD(words)))
                                 (setf (gethash (THIRD(words)) tree) p2))

                               (if eq (gethash (FOURTH(words)) tree) nil
                                 (setf p3 (make-person :name FOURTH(words)
						       :parents (list SECOND(words) THIRD(words))))
                                 (setf (gethash (FOURTH(words)) tree) p3))
                               (print p1)
                               (print p2)
                               (print p3)
))))))
;jamie end

;Nick

(defun sibling(p q)
  (when(=(person-parents p)(person-parents q))
    (append(person-name q))))

(defun half-sibling(p q)
  (when(/=((=(first(person-parents p))(first(person-parents q)))(=(second(person-parents p))(second(person-parents q)))))
    (append(person-name q))))

(defun ancestors(p)
  (when(person-parents p)
    (append(person-parents p)
      (ancestors(first(person-parents p)))
      (ancestors(second(person-parents p))))))
     
;End of Nick

;Ryan

(defun cousin(p1 p2)
	(setq l1 '(ancestor(p1)))
	(setq l2 '(ancestor(p2)))
	(setq bool1 nil) ;;The three boolean values: common, direct, result
	(setq bool2 nil) 
	(setq bool3 nil)
	(dolist (name1 'l1)
		(dolist (name2 'l2)
			(when (= name1 name2) (setq bool1 t))))
	(dolist (n 'l1)
		(when (= n p2-name) (setq bool2 t)))
	(dolist (n2 'l2)
		(when (= n2 p1-name) (setq bool2 t)))
	(when (and (not (not bool)) (not (not bool2)) (setq bool3 t)))
	(return bool3))

;End of Ryan
