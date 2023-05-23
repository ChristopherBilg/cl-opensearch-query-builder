;;;; cl-opensearch-query-builder.lisp

(in-package :cl-opensearch-query-builder)

(defclass opensearch-query-builder ()
  "A class for building OpenSearch queries."
  ((query
    :accessor query
    :initarg :query
    :initform "")
   (index
    :accessor index
    :initarg :index
    :initform "")
   (type
    :accessor type
    :initarg :type
    :initform "")))

(defmethod set-query ((osqb opensearch-query-builder) new-query)
  (setf (query osqb) new-query))

(defmethod to-json ((osqb opensearch-query-builder))
  (json:encode-json-to-string
   `(:query ,(query osqb) :index ,(index osqb) :type ,(type osqb))))

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
