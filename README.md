# cl-opensearch-query-builder

Common Lisp implementation of a builder for the OpenSearch query DSL

## Example Usage

```lisp
(ql:quickload :cl-opensearch-query-builder)
(use-package :cl-opensearch-query-builder)

(defparameter *osqb* (make-instance 'opensearch-query-builder))

(add-must-query *osqb* "field1" "value1")
(add-must-query *osqb* "field2" "value2")
(add-must-query *osqb* "field3" "value3")

(add-must-not-query *osqb* "field4" "value4")
(add-must-not-query *osqb* "field5" "value5")
(add-must-not-query *osqb* "field6" "value6")

(add-should-query *osqb* "field7" "value7")
(add-should-query *osqb* "field8" "value8")
(add-should-query *osqb* "field9" "value9")

(add-filter-query *osqb* "field10" "value10")
(add-filter-query *osqb* "field11" "value11")
(add-filter-query *osqb* "field12" "value12")

(add-sort-by-query *osqb* "field13" "asc")
(add-sort-by-query *osqb* "field14" "desc")
(add-sort-by-query *osqb* "field15" "asc")

(set-pagination-from *osqb* 100)
(set-pagination-size *osqb* 50)

(to-stringified-json *osqb*)
```
