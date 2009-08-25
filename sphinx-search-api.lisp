;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:com.oppermannen.sphinx-search-api)


(defclass sphinx-client ()
  ((host
    :accessor host
    :initarg :host
    :initform "localhost"
    :documentation "searchd host (default is 'localhost')")
   (port
    :accessor port
    :initarg :port
    :initform 3312
    :documentation "searchd port (default is 3312)")
   (path
    :accessor path
    :initarg :path
    :initform ()
    :documentation "searchd unix-domain socket path")
   (socket
    :accessor socket
    :initarg :socket
    :initform ()
    :documentation "searchd unix-domain socket")
   (offset
    :accessor offset
    :initarg :offset
    :initform 0
    :documentation "how much records to seek from result-set start (default is 0)")
   (limit
    :accessor limit
    :initarg :limit
    :initform 20
    :documentation "how much records to return from result-set starting at offset (default is 20)")
   (mode
    :accessor mode
    :initarg :mode
    :initform +sph-match-all+
    :documentation "query matching mode (default is +sph-match-all+)")
   (weights
    :accessor weights
    :initarg :weights
    :initform ()
    :documentation "per-field weights (default is 1 for all fields)")
   (sort
    :accessor sort
    :initarg :sort
    :initform +sph-sort-relevance+
    :documentation "match sorting mode (default is +sph-sort-relevance+)")
   (sortby
    :accessor sortby
    :initarg :sortby
    :initform ""
    :documentation "attribute to sort by (defualt is '')")
   (min_id
    :accessor min_id
    :initarg :min_id
    :initform 0
    :documentation "min ID to match (default is 0)")
   (max_id
    :accessor max_id
    :initarg :max_id
    :initform ()
    :documentation "max ID to match (default is max value for uint on system)")
   (filters
    :accessor filters
    :initarg :filters
    :initform ()
    :documentation "search filters")
   (groupby
    :accessor groupby
    :initarg :groupby
    :initform ""
    :documentation "group-by attribute name")
   (groupfunc
    :accessor groupfunc
    :initarg :groupfunc
    :initform +sph-groupby-day+
    :documentation "group-by function (to pre-process group-by attribute value with; default +sph-groupby-day+)")
   (groupsort
    :accessor groupsort
    :initarg :groupsort
    :initform "@group desc"
    :documentation "group-by sorting clause (to sort groups in result set with; default '@group desc')")
   (groupdistinct
    :accessor groupdistinct
    :initarg :groupdistinct
    :initform ""
    :documentation "group-by count-distinct attribute")
   (maxmatches
    :accessor maxmatches
    :initarg :maxmatches
    :initform 1000
    :documentation "max matches to retrieve (default is 1000)")
   (cutoff
    :accessor cutoff
    :initarg :cutoff
    :initform ()
    :documentation "cutoff to stop searching at")
   (retrycount
    :accessor retrycount
    :initarg :retrycount
    :initform 0
    :documentation "distributed retry count")
   (retrydelay
    :accessor retrydelay
    :initarg :retrydelay
    :initform 0
    :documentation "distributed retry delay")
   (anchor
    :accessor anchor
    :initarg :anchor
    :initform ()
    :documentation "geographical anchor point")
   (indexweights
    :accessor indexweights
    :initarg :indexweights
    :initform ()
    :documentation "per-index weights")
   (ranker
    :accessor ranker
    :initarg :ranker
    :initform +sph-rank-proximity-bm25+
    :documentation "ranking mode (default is +sph-rank-proximity-bm25+)")
   (maxquerytime
    :accessor maxquerytime
    :initarg :maxquerytime
    :initform 0
    :documentation "max query time, milliseconds (default is 0, do not limit)")
   (fieldweights
    :accessor fieldweights
    :initarg :fieldweights
    :initform ()
    :documentation "per-field-name weights")
   (overrides
    :accessor overrides
    :initarg :overrides
    :initform ()
    :documentation "per-query attribute values overrides")
   (select
    :accessor select
    :initarg :select
    :initform "*"
    :documentation "select-list (attributes or expressions, with optional aliases)")
   (last-error
    :accessor last-error
    :initarg :last-error
    :initform ""
    :documentation "last error message")
   (last-warning
    :accessor last-warning
    :initarg :last-warning
    :initform ""
    :documentation "last warning message")
   (reqs
    :accessor reqs
    :initarg :reqs
    :initform ()
    :documentation "requests array for multi-query"))

