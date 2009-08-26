;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)

(defpackage #:com.oppermannen.sphinx-search-api-asd
  (:use :cl :asdf))

(asdf:operate 'asdf:load-op :ieee-floats)
(asdf:operate 'asdf:load-op :cl-pack)



(in-package #:com.oppermannen.sphinx-search-api-asd)

(defsystem #:sphinx-search-api
  :name "SPHINX-SEARCH-API"
  :version "0.0.1"
  :maintainer "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :author "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :licence "To be determined"
  :description "Classifier based on bayes theorem"
  :long-description "SPHINX-SEARCH-API is the Common Lisp connection layer to Sphinx Search <http://sphinxsearch.com/>"
  :serial t
  :components ((:file "package")
               (:file "sphinx-search-api-config")
               (:file "constants")
               (:file "sphinx-search-api"))
  :depends-on (:iolib.sockets :cl-pack))
