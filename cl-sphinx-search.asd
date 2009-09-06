;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-sphinx-search-asd
  (:use :cl :asdf))

(asdf:operate 'asdf:load-op :ieee-floats)
(asdf:operate 'asdf:load-op :cl-pack)


(in-package #:cl-sphinx-search-asd)

(defsystem #:cl-sphinx-search
  :name "CL-SPHINX-SEARCH"
  :version "0.0.1"
  :maintainer "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :author "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :licence "To be determined"
  :description ""
  :long-description "CL-SPHINX-SEARCH is the Common Lisp connection layer to Sphinx Search <http://sphinxsearch.com/>"
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "cl-sphinx-search"))
  :depends-on (:iolib.sockets
               :cl-pack
               :babel))
