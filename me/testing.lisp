(defstruct person name parents) ;Maybe

(defun setparent(p)
   (when(person-parents p)
     (append(person-parents p))))

(defun sibling(p q)
  (when(=(person-parents p)(person-parents q)) ;maybe, lisp logic is weird
    (append(person-name q))))

(defun half-sibling(p q)
  (when(;exclusive or, possibly

(defun ancestors(p)
  (when(person-parents p)
    (append(person-parents p)
      (ancestors(first(person-parents p)))
      (ancestors(second(person-parents p))))))
