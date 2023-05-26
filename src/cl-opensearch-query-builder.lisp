;;;; cl-opensearch-query-builder.lisp

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

(defmethod enable-match-all ((osqb opensearch-query-builder))
  "Enable the 'match_all' parameter of the query."
  (setf (match-all osqb) t))

(defmethod set-from ((osqb opensearch-query-builder) new-from)
  "Set the 'from' parameter of the query."
  (setf (from osqb) new-from))

(defmethod set-size ((osqb opensearch-query-builder) new-size)
  "Set the 'size' parameter of the query."
  (setf (size osqb) new-size))

(defmethod add-filter ((osqb opensearch-query-builder) field value &optional (term *default-filter-term*))
  "Add a 'filter' to the list of query filters."
  (disable-match-all osqb)
  (push (list field value term) (filter osqb)))

(defmethod add-sort-by ((osqb opensearch-query-builder) field order)
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

(defmethod to-stringified-json ((osqb opensearch-query-builder))
  "Return a cl-json JSON object representing the query using the cl-json package."
  (let ((json (make-hash-table)))
    (setf (gethash "from" json) (from osqb))
    (setf (gethash "size" json) (size osqb))
    (setf (gethash "match_all" json) (match-all osqb))
    (setf (gethash "minimum_should_match" json) (minimum-should-match osqb))
    (setf (gethash "filter" json) (filter osqb))
    (setf (gethash "sort" json) (sort-by osqb))
    (setf (gethash "must" json) (must osqb))
    (setf (gethash "must_not" json) (must-not osqb))
    (setf (gethash "should" json) (should osqb))
    (cl-json:encode-json-to-string json)))

;; Example usage:
;;
;; (defparameter *osqb* (make-instance 'opensearch-query-builder))
;; (to-stringified-json *osqb*)
