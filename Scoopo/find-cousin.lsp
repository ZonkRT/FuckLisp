(defun cousin(p1 p2)
	(setq l1 (ancestor(p1)))
	(setq l2 (ancestor(p2)))
	(setq bool1 nil) ;;The three boolean values: common, direct, result
	(setq bool2 nil) ;;not sure how to do boolean so using just 1 and 0
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
	