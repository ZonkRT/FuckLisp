;Nick

(defstruct person name parents)

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
