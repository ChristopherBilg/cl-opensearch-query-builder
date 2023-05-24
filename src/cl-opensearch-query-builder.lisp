;;;; cl-opensearch-query-builder.lisp

(in-package :cl-opensearch-query-builder)

(defparameter *initial-query* (make-hash-table))
(defparameter *initial-filter* (make-hash-table))
(defparameter *initial-sort* (make-hash-table))
(defparameter *initial-from* 0)
(defparameter *initial-size* 10)
(defparameter *initial-match-all* t)
(defparameter *initial-minimum-should-match* 0)

(defclass opensearch-query-builder ()
  "A class for building OpenSearch queries."
  ((query
    :accessor query
    :initarg :query
    :initform *initial-query*)
   (filter
    :accessor filter
    :initarg :filter
    :initform *initial-filter*)
   (sort
    :accessor sort
    :initarg :sort
    :initform *initial-sort*)
   (from
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
    :initform *initial-minimum-should-match*)))

(defmethod initialize-instance :after ((osqb opensearch-query-builder) &key)
  "Initialize the instance."
  (setf (query osqb) *initial-query*)
  (setf (sort osqb) *initial-sort*)
  (setf (from osqb) *initial-from*)
  (setf (size osqb) *initial-size*)
  (setf (match-all osqb) *initial-match-all*)
  (setf (minimum-should-match osqb) *initial-minimum-should-match*))

(defmethod add-query ((osqb opensearch-query-builder) field value)
  "Add a query field to the query."
  (setf (gethash field (query osqb)) value))

(defmethod add-filter ((osqb opensearch-query-builder) field value)
  "Add a filter field to the query."
  (setf (gethash field (filter osqb)) value))

(defmethod add-sort ((osqb opensearch-query-builder) field order)
  "Add a sort field to the query."
  (setf (gethash field (sort osqb)) order))

(defmethod set-from ((osqb opensearch-query-builder) new-from)
  "Set the 'from' parameter of the query."
  (setf (from osqb) new-from))

(defmethod set-size ((osqb opensearch-query-builder) new-size)
  "Set the 'size' parameter of the query."
  (setf (size osqb) new-size))

(defmethod toggle-match-all ((osqb opensearch-query-builder))
  "Toggle the 'match_all' parameter of the query."
  (setf (match-all osqb) (not (match-all osqb))))

(defmethod set-minimum-should-match ((osqb opensearch-query-builder) new-minimum-should-match)
  "Set the 'minimum_should_match' parameter of the query."
  (setf (minimum-should-match osqb) new-minimum-should-match))

(defmethod increment-minimum-should-match ((osqb opensearch-query-builder))
  "Increment the 'minimum_should_match' parameter of the query."
  (incf (minimum-should-match osqb)))

(defmethod decrement-minimum-should-match ((osqb opensearch-query-builder))
  "Decrement the 'minimum_should_match' parameter of the query."
  (decf (minimum-should-match osqb)))

(defmethod to-json ((osqb opensearch-query-builder))
  "Return a JSON string representing the query."
  (json:encode-json-to-string
   `(:query ,(query osqb)
     :filter ,(filter osqb)
     :sort ,(sort osqb)
     :from ,(from osqb)
     :size ,(size osqb)
     :match_all ,(match-all osqb)
     :minimum_should_match ,(minimum-should-match osqb))))
