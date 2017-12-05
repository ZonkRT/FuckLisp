(defstruct person()
  (name)
  (parents))

(defun parent(p)
   (when(person-parents p)
     (append(person-parents p))))

(defun sibling(p q)
  (when(=(person-parents p)(person-parents q)) ;maybe, lisp logic is weird
    (append(person-name q))))

(defun half-sibling(p q)
  (when(

(defun ancestor(p)
  (when(person-parents p)
    (append(person-parents p)
      (ancestors(first(person-parents p)))
      (ancestors(second(person-parents p))))))
