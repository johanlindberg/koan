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
	    (let ((lpart (join (subseq title-parts 0 (- (length title-parts) i))))
		  (rpart (join (subseq title-parts i))))
	      (setf (gethash lpart left) (cons title-index (gethash lpart left '())))
	      (setf (gethash rpart right) (cons title-index (gethash rpart right '())))))))
      
      ;; Find overlaps in titles and index them in hash-tables next and prev.
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
  
(defun order-titles (look-ahead next prev)
  "Order all titles by tree-size (in <next> and <prev> combined) with <look-ahead> depth."
  (let ((ordered-titles '()))
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (setf ordered-titles (cons `(,k ,(+ (count-tree-size k look-ahead 'next next)
						     (count-tree-size k look-ahead 'prev prev))) ordered-titles)))
	     next)
    (sort ordered-titles #'> :key #'cadr)))

(defun select-choice-points (children backtrack-limit)
  (let ((result '()))
    (dolist (child children)
      (when (> backtrack-limit 0)
	(decf backtrack-limit)
	(push child result)))
    result))

(defun best-first-search-with-backtracking (choice-points look-ahead backtrack-limit max-count hash)
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
;	(format t "#~D (~D), " (length longest-curr) count)
        (setf count 0))
      (when (> count max-count)
        (return-from best-first-search-with-backtracking longest-curr)))
    longest-curr))

(defun find-longest-chain (filename n look-ahead backtrack-limit max-count)
  (multiple-value-bind (next prev titles)
      (make-xref filename) ; Index and cross reference all titles in <filename>.
    (let ((ordered-titles (order-titles look-ahead next prev))
	  (longest-chain '()))
      (restart-bind ((abort-search #'(lambda ()
                                       (return-from find-longest-chain (values (reverse longest-chain) titles)))
                       :report-function (lambda (stream)
                                          (format stream "Abort search and return current longest chain (~D titles)." (length longest-chain)))))
        (do ((i 0 (+ i 1)))
            ((> i n))
          (let ((longest-curr '()))
            (let ((curr-title (car (nth i ordered-titles))))
              (format t "~&Exploring ~A " (gethash curr-title titles))
              (setf longest-curr ; Search backwards and
                    (best-first-search-with-backtracking `((,curr-title)) look-ahead backtrack-limit max-count prev))
              (setf longest-curr ; continue forwards
                    (best-first-search-with-backtracking `(,(reverse longest-curr)) look-ahead backtrack-limit max-count next))
              (format t "#~D" (length longest-curr))
              (when (> (length longest-curr) (length longest-chain))
                (setf longest-chain longest-curr)
                (format t "*"))))))
      (values (reverse longest-chain) titles))))

(defun solve (filename &key (n 5) (look-ahead 5) (backtrack-limit 10) (max-count 1000000))
  (format t "~&;; Searching ~A for longest chain of overlapping movie titles." filename)
  (format t "~&;; Explores the ~D most connected titles." n)
  (format t "~&;; Settings: Look-ahead depth = ~D, Backtrack-limit = ~D, Cutoff = ~D." look-ahead backtrack-limit max-count)
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (longest titles)
        (find-longest-chain filename n look-ahead backtrack-limit max-count)
      (print-longest-chain longest titles)
      (format t "~&;; Total time spent: ~,2,,,F" (/ (- (get-internal-real-time) start) INTERNAL-TIME-UNITS-PER-SECOND))
    (length longest))))

