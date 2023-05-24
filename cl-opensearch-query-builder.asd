;;;; cl-opensearch-query-builder.asd

(in-package :asdf)

(defsystem :cl-opensearch-query-builder
  :serial t
  :name "cl-opensearch-query-builder"
  :version "0.0.4"
  :description "Common Lisp implementation of a builder for the OpenSearch query DSL"
  :long-description "Common Lisp implementation of a builder for the OpenSearch query DSL"
  :author "Christopher R. Bilger <christopherbilg@gmail.com>"
  :maintainer "Christopher R. Bilger <christopherbilg@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/ChristopherBilg/cl-opensearch-query-builder"
  :source-control (:git "git@github.com:ChristopherBilg/cl-opensearch-query-builder.git")
  :bug-tracker "https://github.com/ChristopherBilg/cl-opensearch-query-builder/issues"
  :depends-on (:cl-json)
  :pathname "src/"
  :components ((:file "package")
               (:file "cl-opensearch-query-builder")))
