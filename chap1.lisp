(defparameter *titles*
  '(Mr Mrs Jr Miss Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name")


(defun last-name (name)
  "Select the last name from a name represented as a list"
  (let ((reversed-name (reverse name)))
    (if (member (first reversed-name) *titles*)
	(last-name (reverse (rest reversed-name)))
	(first reversed-name))))
