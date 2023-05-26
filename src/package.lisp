;;;; package.lisp

(in-package :cl-user)

(defpackage :cl-opensearch-query-builder
  (:use :cl :asdf)
  (:export
    #:opensearch-query-builder
    #:initialize-instance
    #:set-from
    #:set-size
    #:add-filter
    #:add-sort-by
    #:add-must-query
    #:add-must-not-query
    #:add-should-query
    #:to-stringified-json))
