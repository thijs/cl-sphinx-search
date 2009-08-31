;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)

(defpackage #:com.oppermannen.sphinx-search-api-test
  (:nicknames :sphinx-search-api-test)
  (:use :cl
        :fiveam
        :cl-pack
        :babel
        :iolib.sockets
        :alexandria
        :com.oppermannen.sphinx-search-api))
