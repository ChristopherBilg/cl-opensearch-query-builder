# cl-opensearch-query-builder

Common Lisp implementation of a builder for the OpenSearch query DSL

## Example Usage

```lisp
(ql:quickload :cl-opensearch-query-builder)
(use-package :cl-opensearch-query-builder)

(defparameter *osqb* (make-instance 'opensearch-query-builder))
(set-query *osqb* "foo")
(to-json *osqb*)

(let ((osqb (make-instance 'opensearch-query-builder)))
  (set-query osqb "my query")
  (set-index osqb "my index")
  (set-type osqb "my type")
  (to-json osqb))
```
