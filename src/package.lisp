;;;; package.lisp

(in-package :cl-user)

(defpackage :cl-opensearch-query-builder
  (:use :cl :asdf)
  (:export
    #:opensearch-query-builder
    #:initialize-instance
    #:set-from
    #:set-size
    #:to-stringified-json))
