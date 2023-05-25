;;;; cl-opensearch-query-builder.lisp

(in-package :cl-opensearch-query-builder)

(defparameter *initial-from* 0)
(defparameter *initial-size* 10)
(defparameter *initial-match-all* t)
(defparameter *initial-minimum-should-match* 0)

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
    :initform *initial-minimum-should-match*)))

(defmethod initialize-instance :after ((osqb opensearch-query-builder) &key)
  "Initialize the instance."
  (setf (from osqb) *initial-from*)
  (setf (size osqb) *initial-size*)
  (setf (match-all osqb) *initial-match-all*)
  (setf (minimum-should-match osqb) *initial-minimum-should-match*))

(defmethod disable-match-all ((osqb opensearch-query-builder))
  "Disable the 'match_all' parameter of the query."
  (setf (match-all osqb) nil))

(defmethod set-from ((osqb opensearch-query-builder) new-from)
  "Set the 'from' parameter of the query."
  (setf (from osqb) new-from))

(defmethod set-size ((osqb opensearch-query-builder) new-size)
  "Set the 'size' parameter of the query."
  (setf (size osqb) new-size))

(defmethod to-stringified-json ((osqb opensearch-query-builder))
  "Return a cl-json JSON object representing the query using the cl-json package."
  (let ((json (make-hash-table)))
    (setf (gethash "from" json) (from osqb))
    (setf (gethash "size" json) (size osqb))
    (setf (gethash "match_all" json) (match-all osqb))
    (setf (gethash "minimum_should_match" json) (minimum-should-match osqb))
    (cl-json:encode-json-to-string json)))

;; Example usage:
(defparameter *osqb* (make-instance 'opensearch-query-builder))

(to-stringified-json *osqb*)
