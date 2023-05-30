;;;; src/cl-opensearch-query-builder.lisp

(in-package :cl-opensearch-query-builder)

(defparameter *initial-from* 0)
(defparameter *initial-size* 10)
(defparameter *initial-match-all* t)
(defparameter *initial-minimum-should-match* 0)
(defparameter *initial-filter* '())
(defparameter *initial-sort-by* '())
(defparameter *initial-must* '())
(defparameter *initial-must-not* '())
(defparameter *initial-should* '())

(defparameter *default-filter-term* "term")
(defparameter *default-must-term* "match")
(defparameter *default-must-not-term* "match")
(defparameter *default-should-term* "match")
(defparameter *default-should-increment-value* 1)

(defclass opensearch-query-builder ()
  ((from
    :accessor from
    :initarg :from
    :initform *initial-from*)
   (size
    :accessor size
    :initarg :size
    :initform *initial-size*)
   (match-all
    :accessor match-all
    :initarg :match-all
    :initform *initial-match-all*)
   (minimum-should-match
    :accessor minimum-should-match
    :initarg :minimum-should-match
    :initform *initial-minimum-should-match*)
   (filter
    :accessor filter
    :initarg :filter
    :initform *initial-filter*)
   (sort-by
    :accessor sort-by
    :initarg :sort-by
    :initform *initial-sort-by*)
   (must
    :accessor must
    :initarg :must
    :initform *initial-must*)
   (must-not
    :accessor must-not
    :initarg :must-not
    :initform *initial-must-not*)
   (should
    :accessor should
    :initarg :should
    :initform *initial-should*)))

(defmethod initialize-instance :after ((osqb opensearch-query-builder) &key)
  "Initialize the instance."
  (setf (from osqb) *initial-from*)
  (setf (size osqb) *initial-size*)
  (setf (match-all osqb) *initial-match-all*)
  (setf (minimum-should-match osqb) *initial-minimum-should-match*)
  (setf (filter osqb) *initial-filter*)
  (setf (sort-by osqb) *initial-sort-by*)
  (setf (must osqb) *initial-must*)
  (setf (must-not osqb) *initial-must-not*)
  (setf (should osqb) *initial-should*))

(defmethod disable-match-all ((osqb opensearch-query-builder))
  "Disable the 'match_all' parameter of the query."
  (setf (match-all osqb) nil))

(defmethod set-pagination-from ((osqb opensearch-query-builder) new-from)
  "Set the 'from' parameter of the query (for pagination)."
  (setf (from osqb) new-from))

(defmethod set-pagination-size ((osqb opensearch-query-builder) new-size)
  "Set the 'size' parameter of the query (for pagination)."
  (setf (size osqb) new-size))

(defmethod add-filter-query ((osqb opensearch-query-builder) field value &optional (term *default-filter-term*))
  "Add a 'filter' to the list of query filters."
  (disable-match-all osqb)
  (push (list field value term) (filter osqb)))

(defmethod add-sort-by-query ((osqb opensearch-query-builder) field order)
  "Add a 'sort-by' to the list of query sort-bys."
  (push (list field order) (sort-by osqb)))

(defmethod add-must-query ((osqb opensearch-query-builder) field value &optional (term *default-must-term*))
  "Add a 'must' to the list of query musts."
  (disable-match-all osqb)
  (push (list field value term) (must osqb)))

(defmethod add-must-not-query ((osqb opensearch-query-builder) field value &optional (term *default-must-not-term*))
  "Add a 'must-not' to the list of query must-nots."
  (disable-match-all osqb)
  (push (list field value term) (must-not osqb)))

(defmethod add-should-query ((osqb opensearch-query-builder) field value &optional (term *default-should-term*) (increment-value *default-should-increment-value*))
  "Add a 'should' to the list of query shoulds."
  (disable-match-all osqb)
  (if increment-value
      (setf (minimum-should-match osqb)
            (+ (minimum-should-match osqb) increment-value)))
  (push (list field value term) (should osqb)))

(defun append-hash-tables (first &rest rest)
  "Append all hash tables to the first hash table."
  (loop for hash-table in rest
        do (loop for key being the hash-keys of hash-table
                 using (hash-value value)
                 do (setf (gethash key first) value)))
  first)

(defmethod handle-minimum-should-match ((osqb opensearch-query-builder) minimum-should-match-hash)
  "Handle the 'minimum-should-match' parameter of the query."
  (setf (gethash "minimum_should_match" minimum-should-match-hash) (minimum-should-match osqb)))

(defmethod handle-filter ((osqb opensearch-query-builder) filter-hash)
  "Handle the 'filter' parameter of the query."
  (let ((filter-list (list)))
    (dolist (filter-query (filter osqb))
      (let ((filter-query-hash (make-hash-table)))
        (setf (gethash (third filter-query) filter-query-hash)
              (make-hash-table :test 'equal))
        (setf (gethash (first filter-query) (gethash (third filter-query) filter-query-hash))
              (second filter-query))
        (push filter-query-hash filter-list)))
    (setf (gethash "filter" filter-hash) filter-list)))

(defmethod handle-sort-by ((osqb opensearch-query-builder) sort-by-hash)
  "Handle the 'sort-by' parameter of the query."
  (let ((sort-by-list (list)))
    (dolist (sort-by-query (sort-by osqb))
      (let ((sort-by-query-hash (make-hash-table)))
        (setf (gethash (first sort-by-query) sort-by-query-hash)
              (second sort-by-query))
        (push sort-by-query-hash sort-by-list)))
    (setf (gethash "sort" sort-by-hash) sort-by-list)))

(defmethod handle-must ((osqb opensearch-query-builder) must-hash)
  "Handle the 'must' parameter of the query."
  (let ((must-list (list)))
    (dolist (must-query (must osqb))
      (let ((must-query-hash (make-hash-table)))
        (setf (gethash (third must-query) must-query-hash)
              (make-hash-table :test 'equal))
        (setf (gethash (first must-query) (gethash (third must-query) must-query-hash))
              (second must-query))
        (push must-query-hash must-list)))
    (setf (gethash "must" must-hash) must-list)))

(defmethod handle-must-not ((osqb opensearch-query-builder) must-not-hash)
  "Handle the 'must-not' parameter of the query."
  (let ((must-not-list (list)))
    (dolist (must-not-query (must-not osqb))
      (let ((must-not-query-hash (make-hash-table)))
        (setf (gethash (third must-not-query) must-not-query-hash)
              (make-hash-table :test 'equal))
        (setf (gethash (first must-not-query) (gethash (third must-not-query) must-not-query-hash))
              (second must-not-query))
        (push must-not-query-hash must-not-list)))
    (setf (gethash "must_not" must-not-hash) must-not-list)))

(defmethod handle-should ((osqb opensearch-query-builder) should-hash)
  "Handle the 'should' parameter of the query."
  (let ((should-list (list)))
    (dolist (should-query (should osqb))
      (let ((should-query-hash (make-hash-table)))
        (setf (gethash (third should-query) should-query-hash)
              (make-hash-table :test 'equal))
        (setf (gethash (first should-query) (gethash (third should-query) should-query-hash))
              (second should-query))
        (push should-query-hash should-list)))
    (setf (gethash "should" should-hash) should-list)))

(defmethod to-stringified-json ((osqb opensearch-query-builder))
  "Return a stringified JSON object representing the query."
  (let ((json (make-hash-table)))
    (setf (gethash "from" json) (from osqb))
    (setf (gethash "size" json) (size osqb))

    (when (match-all osqb)
      (setf (gethash "match_all" json) (match-all osqb)))

    (let ((bool-hash (make-hash-table))
          (minimum-should-match-hash (make-hash-table))
          (filter-hash (make-hash-table))
          (sort-by-hash (make-hash-table))
          (must-hash (make-hash-table))
          (must-not-hash (make-hash-table))
          (should-hash (make-hash-table)))
      (when (minimum-should-match osqb)
        (handle-minimum-should-match osqb minimum-should-match-hash))

      (when (filter osqb)
        (handle-filter osqb filter-hash))

      (when (sort-by osqb)
        (handle-sort-by osqb sort-by-hash))

      (when (must osqb)
        (handle-must osqb must-hash))

      (when (must-not osqb)
        (handle-must-not osqb must-not-hash))

      (when (should osqb)
        (handle-should osqb should-hash))

      (setf (gethash "bool" bool-hash) (append-hash-tables minimum-should-match-hash filter-hash sort-by-hash must-hash must-not-hash should-hash))
      (setf (gethash "query" json) bool-hash))

    (com.inuoe.jzon:stringify json)))
