;;;; package.lisp

(in-package :cl-user)

(defpackage :cl-opensearch-query-builder
  (:use :cl :asdf)
  (:export
    #:opensearch-query-builder
    #:initialize-instance
    #:add-query
    #:add-filter
    #:add-sort
    #:set-from
    #:set-size
    #:to-json))
