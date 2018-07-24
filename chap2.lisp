(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase ->(Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")


(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially,this is
  *simple-grammar*, but we can switch to other grammars.")


(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))


(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))


(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))


(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))


(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
	 (mappend #'generate phrase))
	((rewrites phrase)
	 (generate (random-elt (rewrites phrase))))
	(t (list phrase))))


(defun generate1 (phrase)
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase)
	   (mappend #'generate1 phrase))
	  (choices (generate1 (random-elt choices)))
	  (t (list phrase)))))


(defun generate2 (phrase)
  (let ((choices (rewrites phrase)))
    (if (not choices)
	(list phrase)
	(if (listp (first choices))
	    (mappend #'generate2 (first choices))
	    (generate2 (random-elt choices))))))


(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
	((listp phrase)
	 (combine-all (generate-all (first phrase))
		      (generate-all (rest phrase)))) 
	((rewrites phrase)
	 (mappend #'generate-all (rewrites phrase)))
	(t (list (list phrase)))))


(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 21))."
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (append x y)) xlist))
	   ylist))


(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #' (lambda (y)
		(mapcar #'(lambda (x) (funcall fn x y))
			xlist))
	      ylist))


(defun combine-all2 (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))
