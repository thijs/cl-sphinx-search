;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)

(defpackage #:com.oppermannen.sphinx-search-api
  (:nicknames "sphinx-search-api")
  (:use :cl :iolib.sockets :babel :cl-pack)
  (:export #:set-server
           #:set-limits
           #:query
           #:add-query
           #:run-queries
           #:get-last-error
           #:get-last-warning)
  (:documentation
   "This package provides an interface to the search daemon (@em{searchd})
    for @a[http://www.sphinxsearch.com/]{Sphinx}.

    @begin[About Sphinx]{section}

    From the site:

    @begin{pre}
    Sphinx is a full-text search engine, distributed under GPL version 2.
    Commercial license is also available for embedded use.

    Generally, it's a standalone search engine, meant to provide fast,
    size-efficient and relevant fulltext search functions to other applications.
    Sphinx was specially designed to integrate well with SQL databases and
    scripting languages. Currently built-in data sources support fetching data
    either via direct connection to MySQL or PostgreSQL, or using XML pipe
    mechanism (a pipe to indexer in special XML-based format which Sphinx
    recognizes).

    As for the name, Sphinx is an acronym which is officially decoded as
    SQL Phrase Index. Yes, I know about CMU's Sphinx project.
    @end{pre}
    @end{section}

    @begin[Synopsis]{section}
    @begin{pre}

    (let ((sph (make-instance 'sphinx-client)))
      (add-query sph \"test\")
      (run-queries sph))

    @end{pre}
    @end{section}

    @begin[One class]{section}
    There is just one class:

    @aboutclass{sphinx-client}
    @end{section}

    @begin[Methods]{section}
    Setting options/parameters:

    @aboutfun{set-server}
    @aboutfun{set-limits}

    Running queries:

    @aboutfun{query}
    @aboutfun{add-query}
    @aboutfun{run-queries}

    @end{section}
"))

