;;;; cl-opensearch-query-builder.lisp

(in-package :cl-opensearch-query-builder)

(defparameter *initial-query* "")
(defparameter *initial-index* "")
(defparameter *initial-sort* "")
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
   (index
    :accessor index
    :initarg :index
    :initform *initial-index*)
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

;; Example class instance methods:
;;
;; (defmethod set-query ((osqb opensearch-query-builder) new-query)
;;   (setf (query osqb) new-query))

;; (defmethod to-json ((osqb opensearch-query-builder))
;;   (json:encode-json-to-string
;;    `(:query ,(query osqb) :index ,(index osqb) :type ,(type osqb))))

;; Example usage:
;;
;; (defparameter *osqb* (make-instance 'opensearch-query-builder))
;; (set-query *osqb* "foo")
;; (to-json *osqb*)

;; (let ((osqb (make-instance 'opensearch-query-builder)))
;;   (set-query osqb "my query")
;;   (set-index osqb "my index")
;;   (set-type osqb "my type")
;;   (to-json osqb))
