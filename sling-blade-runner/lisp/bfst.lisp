;;; January 2010 attempt on MOVIES.LST
;;; Best-first search with backtracking
;;; using several threads.

(defparameter *available-processes* '())
(defparameter *longest-chain* '())
(defparameter *titles* '())

(defparameter *available-processes-lock* (make-lock))
(defparameter *longest-chain-lock* (make-lock))
(defparameter *titles-lock* (make-lock))

(defun split (string &optional (delimiter #\SPACE))
  "Splits <string> into pieces separated by <delimiter>."
  ;; Stolen from the Common Lisp Cookbook. See
  ;; http://cl-cookbook.sourceforge.net/strings.html
  (loop for i = 0 then (1+ j)
     as j = (position delimiter string :start i)
     collect (subseq string i j)
     while j))

(defun join (sequence &optional (delimiter " "))
  "Joins <sequence> into a string separated by <delimiter>."
  (let ((result (car sequence)))
    (when (cdr sequence)
      (mapcar #'(lambda (x)
		  (setf result (concatenate 'string result delimiter x)))
	      (cdr sequence)))
    result))

(defun print-longest-chain ()
  "Print the longest chain found to stdout."
  (format t "~&Longest chain is ~A titles~%" (length *longest-chain*))
  (let ((i 1))
    (mapcar #'(lambda (index)
		(format t "~3D ~A~%" i (gethash index *titles*))
		(incf i))
	    *longest-chain*)))

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

      (setf *titles* titles)

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
			   (setf (gethash left-index prev)
                                 (cons right-index (gethash left-index prev '())))
			   (setf (gethash right-index next)
                                 (cons left-index (gethash right-index next '()))))))))
	       left)
      (values next prev))))

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

(defun best-first-search-with-backtracking (choice-points look-ahead backtrack-limit max-count hash)
  (let ((longest-curr '())
	(result '())
        (count 0)
	(curr-index nil))
    (do () ((not choice-points))
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

(defun search-from-title (curr-title look-ahead backtrack-limit cutoff prev next)
  (let ((longest-curr '()))
    (setf longest-curr
          (best-first-search-with-backtracking
           `((,curr-title)) look-ahead backtrack-limit cutoff prev))
    (setf longest-curr
          (best-first-search-with-backtracking
           `(,(reverse longest-curr)) look-ahead backtrack-limit cutoff next))
    (with-lock-grabbed (*longest-chain-lock*)
      (when (> (length longest-curr) (length *longest-chain*))
        (setf *longest-chain* (reverse longest-curr))))))

(defun copy-hash-table (table &key (test nil) (size nil))
  (let* ((test (or test (hash-table-test table)))
         (size (or size (hash-table-size table)))
         (copy (make-hash-table :test test :size size)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall 'identity v)))
             table)
    copy))

(defun find-longest-chain (filename top processes look-ahead backtrack-limit cutoff)
  (multiple-value-bind (next prev)
      (make-xref filename) ; Index and cross reference all titles in <filename>.
    (let ((ordered-titles (order-titles look-ahead next prev)))
      (restart-bind ((abort-search #'(lambda ()
                                       (return-from find-longest-chain
                                         (values *longest-chain* *titles*)))
                       :report-function (lambda (stream)
                                          (format stream
                                                  "Abort search and return current longest chain (~D titles)."
                                                  (length *longest-chain*)))))
        (dotimes (i top)
          (let ((curr-title (car (nth i ordered-titles)))
                (copy-i i))
            (with-lock-grabbed (*available-processes-lock*)
              (decf *available-processes*))
            (process-run-function (format nil "search~D" copy-i)
                                  #'(lambda ()
                                      (with-lock-grabbed (*titles-lock*)
                                        (format t "~&[~D] Exploring ~A" copy-i (gethash curr-title *titles*)))
                                      (let ((result (search-from-title curr-title
                                                                       look-ahead backtrack-limit cutoff
                                                                       (copy-hash-table prev)
                                                                       (copy-hash-table next))))
                                        (with-lock-grabbed (*titles-lock*)
                                          (format t "~&[~D] Exploring ~A #~D" 
                                                  copy-i (gethash curr-title *titles*) (length result)))
                                        (with-lock-grabbed (*available-processes-lock*)
                                          (incf *available-processes*))))))
          (process-wait "waiting for an available thread"
                        #'(lambda ()
                            (with-lock-grabbed (*available-processes-lock*)
                              (>= *available-processes* 1))))))
      (process-wait "waiting for all threads to finish"
                    #'(lambda ()
                        (with-lock-grabbed (*available-processes-lock*)
                          (eq *available-processes* processes))))
      (values *longest-chain* *titles*))))

(defun solve (filename &key (top 5) (processes 2) (look-ahead 5) (backtrack-limit 10) (cutoff 1000000))
  (format t "~&;; Searching ~A for longest chain of overlapping movie titles." filename)
  (format t "~&;; Explores the ~D most connected titles using ~D process(es)." top processes)
  (format t "~&;; Settings: Look-ahead depth = ~D, Backtrack-limit = ~D, Cutoff = ~D." look-ahead backtrack-limit cutoff)
  (with-lock-grabbed (*available-processes-lock*)
    (setf *available-processes* processes))
  (let ((start (get-internal-real-time)))
    (find-longest-chain filename top processes look-ahead backtrack-limit cutoff)
    (print-longest-chain)
    (format t "~&;; Total time spent: ~,2,,,F" (/ (- (get-internal-real-time) start) INTERNAL-TIME-UNITS-PER-SECOND))
    (length *longest-chain*)))

