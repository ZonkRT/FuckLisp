;add in below jamie's, need to figure out how to print query
(if (eq (FIRST words) 'x)

	(when (eq (THIRD words) 'parent)
		(setq parent (gethash (SECOND words tree)))
		(setq kid (gethash (FOURTH words tree)))
		(if (or (eq (first person-parents kid) parent) (second person-parents kid) parent)
			(write 'Yes) (write 'No)))
	
	(when (eq (THIRD words) 'sibling)
		(if (sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
			(write 'Yes) (write 'No)))
		
	;need way to get person from name, run through whole tree?
	(when (eq (THIRD words) 'half-sibling)
		(if (half-sibling (gethash (SECOND words) tree) (gethash (FOURTH words) tree)) 
			(write 'Yes) (write 'No)))
	
	(when (eq (THIRD(words) 'ancestor)
		(setq ancList '(gethash (FOURTH words) tree))
		(setq flag nil)
		(dolist (pa 'ancList)
			(when (eq pa (gethash (SECOND words) tree)) (setq flag t)))
		(if (not (not flag)) (write 'Yes) (write 'No)))
	
	(when (eq (THIRD words) 'cousin)
		(if (not (not (cousin (gethash (SECOND words) tree) (gethash (FOURTH words) tree))))
			(write 'Yes) (write 'No)))
	
	(when (eq (THIRD words) 'unrelated))
)

