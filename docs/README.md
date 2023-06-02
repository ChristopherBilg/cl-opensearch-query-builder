# Documentation

This set of documentation file(s) is aimed at giving users of this Common Lisp package an overview of usage, functionality, etc.

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

## Exported Classes

| Class Name | Description |
| - | - |
| `opensearch-query-builder` | A (builder pattern) CLOS class for programatically creating OpenSearch JSON objects representing a search query. |

## Exported Class Methods

| Class Name | Method Name | Description |
| - | - | - |
| opensearch-query-builder | `initialize-instance` | Create a new CLOS instance of the **opensearch-query-builder** class. |
| opensearch-query-builder | `set-pagination-from` | Set the pagination value to start the query at. |
| opensearch-query-builder | `set-pagination-size` | Set the length of the pagination to obtain the query for. |
| opensearch-query-builder | `add-filter-query` | Add a **filter** clause to the query. |
| opensearch-query-builder | `add-sort-by-query` | Add a **sort** clause to the query. |
| opensearch-query-builder | `add-must-query` | Add a **must** contain clause to the query. |
| opensearch-query-builder | `add-must-not-query` | Add a **must not** contain clause to the query. |
| opensearch-query-builder | `add-should-query` | Add a **should** contain clause to the query. Note: This does not currently support nested should blocks. |
| opensearch-query-builder | `to-stringified-json` | Convert the CLOS instance into a stringified JSON object containing the *supported* [OpenSearch query structure](#supported-opensearch-query-structure). |

## Supported OpenSearch Query Structure

```json
{
    "query": {
        "bool": {
            "from": integer,
            "size": integer,
            "match_all": true | undefined,
            "filter": [
                {
                    term: {
                        field: value
                    }
                },
                ...
            ],
            "sort": [
                {
                    field: {
                        "order": order
                    }
                },
                ...
            ],
            "must": [
                {
                    term: {
                        field: value
                    }
                },
                ...
            ],
            "must_not": [
                {
                    term: {
                        field: value
                    }
                },
                ...
            ],
            "should": [
                {
                    term: {
                        field: value
                    }
                },
                ...
            ],
            "minimum_should_match": integer
        }
    }
}
```
