;;;; src/package.lisp

(in-package :cl-user)

(defpackage :cl-opensearch-query-builder
  (:use :cl :asdf)
  (:export
    #:opensearch-query-builder
    #:initialize-instance
    #:set-pagination-from
    #:set-pagination-size
    #:add-filter-query
    #:add-sort-by-query
    #:add-must-query
    #:add-must-not-query
    #:add-should-query
    #:to-stringified-json))
