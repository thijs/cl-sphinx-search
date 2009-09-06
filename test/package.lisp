;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-sphinx-search-test
  (:nicknames :sphinx-search-api-test)
  (:use :cl
        :fiveam
        :cl-pack
        :babel
        :iolib.sockets
        :alexandria
        :cl-sphinx-search))
