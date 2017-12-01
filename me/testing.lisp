(defstruct person()
  (name)
  (parents))



(defun ancestor(p)
  (when(person-parents p)
    (append(person-parents p)
      (ancestors(first(person-parents p)))
      (ancestors(second(person-parents p))))))
