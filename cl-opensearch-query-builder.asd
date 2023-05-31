;;;; cl-opensearch-query-builder.asd

(in-package :asdf)

(defparameter *cl-opensearch-query-builder-serial* t)
(defparameter *cl-opensearch-query-builder-name* "cl-opensearch-query-builder")
(defparameter *cl-opensearch-query-builder-version* "0.0.10")
(defparameter *cl-opensearch-query-builder-description* "Common Lisp implementation of a builder for the OpenSearch query DSL")
(defparameter *cl-opensearch-query-builder-long-description* "Common Lisp implementation of a builder for the OpenSearch query DSL")
(defparameter *cl-opensearch-query-builder-author* "Christopher R. Bilger <christopherbilg@gmail.com>")
(defparameter *cl-opensearch-query-builder-maintainer* "Christopher R. Bilger <christopherbilg@gmail.com>")
(defparameter *cl-opensearch-query-builder-license* "MIT")
(defparameter *cl-opensearch-query-builder-homepage* "https://github.com/ChristopherBilg/cl-opensearch-query-builder")
(defparameter *cl-opensearch-query-builder-source-control* '(:git "git@github.com:ChristopherBilg/cl-opensearch-query-builder.git"))
(defparameter *cl-opensearch-query-builder-bug-tracker* "https://github.com/ChristopherBilg/cl-opensearch-query-builder/issues")

(defsystem :cl-opensearch-query-builder
  :serial *cl-opensearch-query-builder-serial*
  :name *cl-opensearch-query-builder-name*
  :version *cl-opensearch-query-builder-version*
  :description *cl-opensearch-query-builder-description*
  :long-description *cl-opensearch-query-builder-long-description*
  :author *cl-opensearch-query-builder-author*
  :maintainer *cl-opensearch-query-builder-maintainer*
  :license *cl-opensearch-query-builder-license*
  :homepage *cl-opensearch-query-builder-homepage*
  :source-control *cl-opensearch-query-builder-source-control*
  :bug-tracker *cl-opensearch-query-builder-bug-tracker*
  :depends-on (:com.inuoe.jzon)
  :pathname "src/"
  :components ((:file "package")
               (:file "cl-opensearch-query-builder")))

(defsystem :cl-opensearch-query-builder-test
  :serial *cl-opensearch-query-builder-serial*
  :name (concatenate 'string *cl-opensearch-query-builder-name* "-test")
  :version *cl-opensearch-query-builder-version*
  :description (concatenate 'string *cl-opensearch-query-builder-description* " (test suite)")
  :long-description (concatenate 'string *cl-opensearch-query-builder-long-description* " (test suite)")
  :author *cl-opensearch-query-builder-author*
  :maintainer *cl-opensearch-query-builder-maintainer*
  :license *cl-opensearch-query-builder-license*
  :homepage *cl-opensearch-query-builder-homepage*
  :source-control *cl-opensearch-query-builder-source-control*
  :bug-tracker *cl-opensearch-query-builder-bug-tracker*
  :depends-on (
    :cl-opensearch-query-builder
    :fiveam)
  :pathname "tests/"
  :components ((:file "package")
               (:file "cl-opensearch-query-builder"))
  :perform (test-op :after (op c)
           (asdf:load-system :cl-opensearch-query-builder-test)
           (asdf:test-system :cl-opensearch-query-builder-test)))
