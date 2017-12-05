(defun cousin(p1 p2)
	(setq l1 (ancestor(p1)))
	(setq l2 (ancestor(p2)))
	(setq bool1 0) ;;The three boolean values: common, direct, result
	(setq bool2 0) ;;not sure how to do boolean so using just 1 and 0
	(setq bool3 0)
	(dolist (name1 'l1)
		(dolist (name2 'l2)
			(when (= name1 name2) (setq bool1 1))))
	(dolist (n 'l1)
		(when (= n p2-name) (setq bool2 1)))
	(dolist (n2 'l2)
		(when (= n2 p1-name) (setq bool2 1)))
	(when (and (= bool1 1) (= bool2 1)) (setq bool3 1))
	(return bool3))
	