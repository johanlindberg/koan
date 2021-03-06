;;; Best-first search with backtracking for Sling Blade Runner puzzle.

;;; Jan/Feb 2009  Best first search w/ backtracking attempt.
;;; Aug 2013      Updates, clarifications and implementation of expand-chain.

(defpackage :bfs
  (:use cl)
  (:export :make-xref :order-titles :search-title :print-longest-chain
	   :find-longest-chain :solve))
(in-package :bfs)

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

   Returns a hash-table of indexes and titles and the number of titles
   indexed. The hash-table has movie titles (strings) as values and indexes
   (integers) as keys."
  (let ((titles (make-hash-table))) ; N -> "Foo"
    (with-open-file (movies filename :direction :input)
      (do ((title-index 0 (+ title-index 1))
	   (title (read-line movies) (read-line movies nil 'eof)))
	  ((eq title 'eof) (values titles title-index))
        (setf title (string-trim '(#\Space
                                   #\Tab
                                   #\Newline
                                   #\Return) title))
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

      ;; Split titles into pieces and index them in hash-tables left and right.
      (dotimes (title-index last-index)
	(let ((title-parts (split (gethash title-index titles))))
	  (dotimes (i (length title-parts))
	    (let ((lpart (join (subseq title-parts 
				       0 (- (length title-parts) i))))
		  (rpart (join (subseq title-parts i))))
	      (setf (gethash lpart left)
		    (cons title-index (gethash lpart left '())))
	      (setf (gethash rpart right)
		    (cons title-index (gethash rpart right '())))))))
      
      ;; Find overlaps in titles and index them in hash-tables next and prev.
      (maphash #'(lambda (title-part left-indexes)
		   (when (gethash title-part right)
		     (dolist (left-index left-indexes)
		       (dolist (right-index (gethash title-part right))
			 (unless (eq left-index right-index)
			   (setf (gethash left-index prev)
				 (cons right-index
				       (gethash left-index prev '())))
			   (setf (gethash right-index next)
				 (cons left-index
				       (gethash right-index next '()))))))))
	       left)
      (values next prev titles))))

(let ((memo (make-hash-table :test #'equal)))
  (defun count-tree-size (index depth hash-table-name hash-table)
    "Returns the number of child nodes for <index>.

     All children, grand-children etc down to <depth> are included
     in the count."
    (let ((result (gethash `(,index ,depth ,hash-table-name) memo)))
      (if result
	  result
	  (setf (gethash `(,index ,depth ,hash-table-name) memo)
		(internal-count-tree-size index depth
					  hash-table-name hash-table)))))
  
  (defun internal-count-tree-size (index depth hash-table-name hash-table)
    (let ((children (gethash index hash-table))
	  (result 1))
      (if (and (> depth 0)
	       children)
	  (dolist (child children)
	    (setf result (+ (count-tree-size child (- depth 1)
					     hash-table-name hash-table)
			    result)))
	  (setf result 1))
      result)))

(defun get-children (index depth next visited-indexes)
  "Return a sorted list of all children for <index>.

   Only child-indexes who are not in <visited-indexes> are used."
  (let ((result (sort (remove-if #'(lambda (i)
				     (member i visited-indexes))
				 (gethash index next))
		      #'(lambda (a b)
			  (> (count-tree-size a depth 'next next)
			     (count-tree-size b depth 'next next))))))
    result))
  
(defun order-titles (look-ahead next prev)
  "Order all titles by tree-size (in <next> and <prev> combined)."
  (let ((ordered-titles '()))
    (maphash
     #'(lambda (k v)
	 (declare (ignore v))
	 (setf ordered-titles 
	       (cons `(,k ,(+ (count-tree-size k look-ahead 'next next)
			      (count-tree-size k look-ahead 'prev prev)))
		     ordered-titles)))
     next)
    (sort ordered-titles #'> :key #'cadr)))

(defun select-choice-points (children backtrack-limit)
  (let ((result '()))
    (dolist (child children)
      (when (> backtrack-limit 0)
	(decf backtrack-limit)
	(push child result)))
    result))

(defun best-first-search-with-backtracking
    (choice-points look-ahead backtrack-limit max-count hash)

  (let ((longest-curr '())
	(result '())
        (count 0)
	(curr-index nil))
    (while choice-points
      (setf result (pop choice-points))
      (setf curr-index (car result))
      (do ((children (get-children curr-index look-ahead hash result)
		     (get-children curr-index look-ahead hash result)))
	  ((or (eq curr-index nil) (eq children nil)) t)
	(setf curr-index (car children))
	(dolist (next-index (select-choice-points children backtrack-limit))
	  (push (append (list next-index) result) choice-points))
	(when curr-index
	  (setf result (push curr-index result))))
      (incf count)
      (when (> (length result) (length longest-curr))
	(setf longest-curr result)
        (setf count 0))
      (when (> count max-count)
        (return-from best-first-search-with-backtracking longest-curr)))
    longest-curr))

(let ((search-result '()))
  (defun inner-find-connection (nexts b search-depth used-nodes hash acc)
    (dolist (index nexts)
      (unless (or (member index used-nodes)
		  (member index acc))
	(if (member b (gethash index hash))
	    (push (append acc (list index b)) search-result)
	    (if (eq search-depth 1)
		nil
		(inner-find-connection (gethash index hash)
				       b
				       (- search-depth 1)
				       used-nodes
				       hash
				       (append acc (list index))))))))

  (defun find-connection (a b search-depth used-nodes hash)
    (setf search-result '())
    (let ((result (list a b)))
      (when (> search-depth 1)
	(inner-find-connection (gethash a hash)
			       b 
			       (- search-depth 1)
			       used-nodes
			       hash
			       (list a))
	
	(dolist (res search-result)
	  (when (> (length res) (length result))
	    (setf result res))))

      result)))

(defun expand-chain (chain look-ahead hash)
  (let ((prev-node '())
        (result (list (car chain))))
    (dolist (node chain)
      (when prev-node
        (let ((c (find-connection prev-node node
				  (* look-ahead 5)
				  (append chain result)
				  hash)))
          (setf result (append result (cdr c)))))
      (setf prev-node node))
    result))

(defun search-title (title look-ahead backtrack-limit max-count prev next)
  "Search from <title> and return the longest chain found."
  (let ((longest-chain '()))
    (setf longest-chain ; Search backwards,
	  (best-first-search-with-backtracking `((,title))
					       look-ahead backtrack-limit
					       max-count prev))

    (setf longest-chain ; continue forwards
	  (best-first-search-with-backtracking `(,(reverse longest-chain)) 
					       look-ahead backtrack-limit
					       max-count next))

    (setf longest-chain ; and expand the chain
	  (expand-chain (reverse longest-chain) look-ahead next))

    longest-chain))


(defun find-longest-chain (filename n look-ahead backtrack-limit max-count)
  (multiple-value-bind (next prev titles)
      (make-xref filename) ; Index and cross reference <filename>.
    (let ((ordered-titles (order-titles look-ahead next prev))
	  (longest-chain '()))
      (restart-bind
	  ((abort-search #'(lambda ()
			     (return-from find-longest-chain
			       (values (reverse longest-chain) titles)))
	     :report-function
	     (lambda (stream)
	       (format stream
		       "Abort search and return current longest chain (~D titles)."
		       (length longest-chain)))))

        (do ((i 0 (+ i 1)))
            ((>= i n))
          (let ((longest-curr '()))
            (let ((curr-title (car (nth i ordered-titles))))
              (format t "~&Exploring ~A " (gethash curr-title titles))
	      (setf longest-curr (search-title curr-title
					       look-ahead
					       backtrack-limit
					       max-count
					       prev next))
              (format t "#~D" (length longest-curr))
              (when (> (length longest-curr) (length longest-chain))
                (setf longest-chain longest-curr)
                (format t "*"))))))

      (values longest-chain titles))))

(defun solve (filename &key (n 5) (look-ahead 5) (backtrack-limit 10) (max-count 1000000))
  (format t "~&;; Searching ~A for longest chain of overlapping movie titles." filename)
  (format t "~&;; Explores the ~D most connected titles." n)
  (format t "~&;; Settings: Look-ahead depth = ~D, Backtrack-limit = ~D, Cutoff = ~D."
	  look-ahead backtrack-limit max-count)

  (let ((start (get-internal-real-time)))
    (multiple-value-bind (longest titles)
        (find-longest-chain filename n look-ahead backtrack-limit max-count)
      (print-longest-chain longest titles)
      (format t "~&;; Total time spent: ~,2,,,Fs"
	      (/ (- (get-internal-real-time) start) INTERNAL-TIME-UNITS-PER-SECOND))

    (length longest))))

