;;; January/February 2009 attempt on MOVIES.LST
;;; Best-first search with backtracking

(defun split (string &optional (delimiter #\SPACE))
  "Splits <string> into pieces separated by <delimiter>."
  ;; Stolen from the Common Lisp Cookbook. See
  ;; http://cl-cookbook.sourceforge.net/strings.html
  (loop for i = 0 then (1+ j)
     as j = (position delimiter string :start i)
     collect (subseq string i j)
     while j))

(defmacro while (test &body body) 
  ;; Stolen from On Lisp.
  `(do ()
       ((not ,test))
     ,@body))

(defun join (sequence &optional (delimiter " "))
  "Joins <sequence> into a string separated by <delimiter>."
  (let ((result (car sequence)))
    (when (cdr sequence)
      (mapcar #'(lambda (x)
		  (setf result (concatenate 'string result delimiter x)))
	      (cdr sequence)))
    result))

(defun print-longest-chain (longest titles)
  "Print longest chain to stdout."
  (format t "~&Longest chain is ~A titles~%" (length longest))
  (let ((i 1))
    (mapcar #'(lambda (index)
		(format t "~3D ~A~%" i (gethash index titles))
		(incf i))
	    longest)))

(defun index-titles (filename)
  "Create a hash-table of titles and indexes from <filename>.

   Returns a hash-tables of indexes and titles and the number of titles
   indexed. The hash-table has movie titles (strings) as values and indexes
   (integers) as keys."
  (let ((titles (make-hash-table))) ; N -> "Foo"
    (with-open-file (movies filename :direction :input)
      (do ((title-index 0 (+ title-index 1))
	   (title (read-line movies) (read-line movies nil 'eof)))
	  ((eq title 'eof) (values titles title-index))
	(unless (string-equal title "") ; Ignore empty lines!
	  (setf (gethash title-index titles) title))))))

(defun make-xref (filename)
  "Makes a cross-reference of overlapping titles in <filename>."
  (let ((next (make-hash-table))
	(prev (make-hash-table))
	(left (make-hash-table :test #'equal))
	(right (make-hash-table :test #'equal)))
    (multiple-value-bind (titles last-index)
	(index-titles filename)

      ;; Split titles into pieces and index them in hash-tables.
      (dotimes (title-index last-index)
	(let ((title-parts (split (gethash title-index titles))))
	  (dotimes (i (length title-parts))
	    (let ((lpart (join (subseq title-parts 0 (- (length title-parts) i))))
		  (rpart (join (subseq title-parts i))))
	      (setf (gethash lpart left) (cons title-index (gethash lpart left '())))
	      (setf (gethash rpart right) (cons title-index (gethash rpart right '())))))))
      
      ;; Find overlaps in titles and index them in hash-tables.
      (maphash #'(lambda (title-part left-indexes)
		   (when (gethash title-part right)
		     (dolist (left-index left-indexes)
		       (dolist (right-index (gethash title-part right))
			 (unless (eq left-index right-index)
			   (setf (gethash left-index prev) (cons right-index (gethash left-index prev '())))
			   (setf (gethash right-index next) (cons left-index (gethash right-index next '()))))))))
	       left)
      (values next prev titles))))

(let ((memo (make-hash-table :test #'equal)))
  (defun reset-count-tree-size ()
    (clrhash memo))
  
  (defun count-tree-size (index depth hash-table-name hash-table)
    (let ((result (gethash `(,index ,depth ,hash-table-name) memo)))
      (if result
	  result
	  (setf (gethash `(,index ,depth ,hash-table-name) memo)
		(internal-count-tree-size index depth hash-table-name hash-table)))))
  
  (defun internal-count-tree-size (index depth hash-table-name hash-table)
    (let ((children (gethash index hash-table))
	  (result 1))
      (if (and (> depth 0)
	       children)
	  (dolist (child children)
	    (setf result (+ (count-tree-size child (- depth 1) hash-table-name hash-table) result)))
	  (setf result 1))
      result)))

(defun get-children (index depth next visited-indexes)
  "Return a sorted list (using tree-size of <depth>) of all children for
   <index>. Only child-indexes who are not in <visited-indexes> are used."
  (let ((result (sort (remove-if #'(lambda (i)
				     (member i visited-indexes))
				 (gethash index next))
		      #'(lambda (a b)
			  (> (count-tree-size a depth 'next next)
			     (count-tree-size b depth 'next next))))))
    result))
  
(defun find-longest-chain (filename n look-ahead backtrack-limit)
  (multiple-value-bind (next prev titles)
      (make-xref filename) ; Index and cross reference all titles in <filename>.
    (let ((ordered-titles '())

	  (longest '())
	  (longest-curr '())
	  (choice-points '())
	  (result '())

	  (curr-index '()))

      (restart-bind ((abort-search #'(lambda ()
				       (return-from find-longest-chain (values (reverse longest) titles)))
		       :report-function (lambda (stream)
					  (format stream "Abort search and return current longest chain (~D titles)." (length longest)))))

	;; Order all titles by tree-size (in next and prev combined) with <look-ahead> depth.
	(maphash #'(lambda (k v)
		     (declare (ignore v))
		     (setf ordered-titles (cons `(,k ,(+ (count-tree-size k look-ahead 'next next)
							 (count-tree-size k look-ahead 'prev prev))) ordered-titles)))
		 next)
	(setf ordered-titles (sort ordered-titles #'> :key #'cadr))
      
	;; Best-first search with chronological backtracking
	(do ((i 0 (+ i 1)))
	    ((> i n))
	  next-title-search
	  (restart-bind ((try-next-title #'(lambda ()
					     (setf choice-points '()
						   result '()
						   longest-curr '()
						   curr-index '()
						   i (+ i 1))
					     (go next-title-search))
			   :report-function (lambda (stream)
					      (if (< i (- n 1))
						  (format stream "Try searching next title (~A)" (gethash (car (nth (+ i 1) ordered-titles)) titles))
						  (format stream "= Abort search")))))
	    (format t "Exploring ~A~%" (gethash (car (nth i ordered-titles)) titles))
	      
	    (setf choice-points (list (list (car (nth i ordered-titles)))))
	    (while choice-points
	      (setf result (pop choice-points))
	      (setf curr-index (car result))
	      (do ((children (get-children curr-index look-ahead prev result)
			     (get-children curr-index look-ahead prev result)))
		  ((or (eq curr-index nil) (eq children nil)) t)
		(setf curr-index (car children))
		(let ((c backtrack-limit))
		  (dolist (i (cdr children))
		    (when (> c 0)
		      (decf c)
		      (push (append (list i) result) choice-points))))
		
		(when curr-index
		  (setf result (push curr-index result))))
	      
	      (when (> (length result) (length longest-curr))
		(setf longest-curr result)
		(format t "<~D, " (length longest-curr))))
	    
	    (setf choice-points (list (reverse longest-curr)))
	    (while choice-points
	      (setf result (pop choice-points))
	      (setf curr-index (car result))
	      (do ((children (get-children curr-index look-ahead next result)
			     (get-children curr-index look-ahead next result)))
		  ((or (eq curr-index nil) (eq children nil)) t)
		(setf curr-index (car children))
		(let ((c backtrack-limit))
		  (dolist (i (cdr children))
		    (when (> c 0)
		      (decf c)
		      (push (append (list i) result) choice-points))))
		  
		(when curr-index
		  (setf result (push curr-index result))))
		
	      (if (> (length result) (length longest))
		  (progn
		    (setf longest result)
		    (setf longest-curr result)
		    (format t ">>~D, " (length longest)))
		  (when (> (length result) (length longest-curr))
		    (setf longest-curr result)
		    (format t ">~D, " (length longest-curr)))))))

	(values (reverse longest) titles)))))

(defun solve (filename &key (n 5) (look-ahead 5) (backtrack-limit 5))
  (multiple-value-bind (longest titles)
      (find-longest-chain filename n look-ahead backtrack-limit)
    (print-longest-chain longest titles)
    (length longest)))

