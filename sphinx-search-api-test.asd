;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage #:com.oppermannen.sphinx-search-api-test-asd
  (:use :cl :asdf))

(asdf:operate 'asdf:load-op :fiveam)

(in-package :com.oppermannen.sphinx-search-api-test-asd)


(defsystem #:sphinx-search-api-test
  :name "SPHINX-SEARCH-API-TEST"
  :version "0.0.1"
  :maintainer "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :author "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :licence "To be determined"
  :description "Test suite for SPHINX-SEARCH-API"
  :long-description "this is the test suite system for SPHINX-SEARCH-API"
  :components ((:module "test"
                        :components ((:file "package")
                                     (:file "test" :depends-on ("package")))))
  :depends-on (:iolib.sockets
               :sphinx-search-api))

