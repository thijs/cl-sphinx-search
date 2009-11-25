;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-sphinx-search)


(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defvar *response-length* ())

(defmacro adv-p (n)
  `(setf p (+ p ,n)))


(defgeneric last-error (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{a string; the last error message returned from the @code{searchd}.}

    Get the last error message sent by searchd.
"))


(defgeneric last-warning (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{a string; the last warning message returned from the @code{searchd}.}

    Get the last warning message sent by searchd.
"))


(defgeneric max-query-time (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{a number; the max query time in milliseconds.}

    Get the max query time.
"))


(defgeneric (setf max-query-time) (max-time client)
  (:documentation
   "@arg[max-time]{the max query time in milliseconds Sphinx is allowed to take}
    @arg[client]{a @class{sphinx-client}}
    @return{a number; the max query time in milliseconds.}

    Set the max query time to max-time in milliseconds.
"))


(defclass sphinx-client ()
  ((%host
    :accessor %host
    :initarg :host
    :initform "localhost"
    :documentation "searchd host (default is 'localhost')")
   (%port
    :accessor %port
    :initarg :port
    :initform 3312
    :documentation "searchd port (default is 3312)")
   (%path
    :accessor %path
    :initarg :path
    :initform ()
    :documentation "searchd unix-domain socket path")
   (%socket
    :accessor %socket
    :initarg :socket
    :initform ()
    :documentation "searchd unix-domain socket")
   (%encoding
    :accessor %encoding
    :initarg :encoding
    :initform :utf-8
    :documentation "the encoding used; utf-8 or latin-1 for sbcs")
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
   (match-mode
    :accessor match-mode
    :initarg :match-mode
    :initform +sph-match-all+
    :documentation "query matching match-mode (default is +sph-match-all+)")
   (weights
    :accessor weights
    :initarg :weights
    :initform ()
    :documentation "per-field weights (default is 1 for all fields)")
   (sort-mode
    :accessor sort-mode
    :initarg :sort-mode
    :initform +sph-sort-relevance+
    :documentation "match sorting mode (default is +sph-sort-relevance+)")
   (sort-by
    :accessor sort-by
    :initarg :sort-by
    :initform ""
    :documentation "attribute to sort by (defualt is '')")
   (min-id
    :accessor min-id
    :initarg :min-id
    :initform 0
    :documentation "min ID to match (default is 0)")
   (max-id
    :accessor max-id
    :initarg :max-id
    :initform 0
    :documentation "max ID to match (default is max value for uint on system)")
   (filters
    :accessor filters
    :initarg :filters
    :initform ()
    :documentation "search filters; a list of lists")
   (group-by
    :accessor group-by
    :initarg :group-by
    :initform ""
    :documentation "group-by attribute name")
   (group-function
    :accessor group-function
    :initarg :group-function
    :initform +sph-groupby-day+
    :documentation "group-by function (to pre-process group-by attribute value with; default +sph-groupby-day+)")
   (group-sort
    :accessor group-sort
    :initarg :group-sort
    :initform "@group desc"
    :documentation "group-by sorting clause (to sort groups in result set with; default '@group desc')")
   (group-distinct
    :accessor group-distinct
    :initarg :group-distinct
    :initform ""
    :documentation "group-by count-distinct attribute")
   (max-matches
    :accessor max-matches
    :initarg :max-matches
    :initform 1000
    :documentation "max matches to retrieve (default is 1000)")
   (cutoff
    :accessor cutoff
    :initarg :cutoff
    :initform 0
    :documentation "cutoff to stop searching at")
   (retry-count
    :accessor retry-count
    :initarg :retry-count
    :initform 0
    :documentation "distributed retry count")
   (retry-delay
    :accessor retry-delay
    :initarg :retry-delay
    :initform 0
    :documentation "distributed retry delay")
   (geo-anchor
    :accessor geo-anchor
    :initarg :geo-anchor
    :initform ()
    :documentation "geographical anchor point; fixed length list with '(attrlat lat attrlon lon)")
   (index-weights
    :accessor index-weights
    :initarg :index-weights
    :initform (make-hash-table)
    :documentation "per-index weights")
   (rank-mode
    :accessor rank-mode
    :initarg :rank-mode
    :initform +sph-rank-proximity-bm25+
    :documentation "ranking mode (default is +sph-rank-proximity-bm25+)")
   (max-query-time
    :accessor max-query-time
    :initarg :max-query-time
    :initform 0
    :documentation "max query time, milliseconds (default is 0, do not limit)")
   (field-weights
    :accessor field-weights
    :initarg :field-weights
    :initform (make-hash-table)
    :documentation "per-field-name weights")
   (overrides
    :accessor overrides
    :initarg :overrides
    :initform (make-hash-table)
    :documentation "per-query attribute value overrides")
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
   (status
    :accessor status
    :initarg :status
    :initform ()
    :documentation "status of last query")
   (reqs
    :accessor reqs
    :initarg :reqs
    :initform ()
    :documentation "list of requests for batched query runs"))
  (:documentation
   "@short{The sphinx-search class.}

    @begin{pre}
    (let ((sph (make-instance 'sphinx-client :host \"localhost\" :port 3315)))
       (add-query sph \"test\")
       (run-queries sph))
    @end{pre}

    The interface to the search daemon goes through this class.

    Set options and settings of the search to be performed on an object
    of this class, and then have it perform one search by calling
    @fun{run-query}, or add a number of queries using @fun{add-query} and
    then calling @fun{run-queries}.

    Either get a result hash or a list of result hashes back, or an error
    that can be retrieved with the @fun{last-error} function.

    @see{set-server}
    @see{set-limits}
    @see{set-id-range}
    @see{set-filter}
    @see{set-filter-range}
    @see{set-filter-float-range}
    @see{set-geo-anchor}
    @see{set-group-by}
    @see{set-group-distinct}
    @see{set-select}
    @see{reset-filters}
    @see{reset-group-by}
    @see{reset-overrides)}
    @see{last-warning}
    @see{max-query-time}
"))


(defgeneric set-server (client &key host port path)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[host]{the host to connect to when using an INET socket}
    @arg[port]{the port to connect to when using an INET socket}
    @arg[path]{the path to the unix domain socket when not using INET}
    @return{client}
    @short{Set the server host:port or path to connect to.}

    @begin{pre}
    (set-server client :host host :port port)
    (set-server client :path unix-path)
    @end{pre}

    In the first form, sets the @code{host} (string) and @code{port} (integer)
    details for the searchd server using a network (INET) socket.

    In the second form, where @code{unix-path} is a local filesystem path
    (optionally prefixed by 'unix://'), sets the client to access the
    searchd server via a local (UNIX domain) socket at the specified path.
"))

(defmethod set-server ((client sphinx-client) &key (host "localhost") (port 3312) path)
  (cond (path
         (assert (stringp path))
         (when (string= path "unix://" :start1 0 :end1 7)
           (setf path (subseq path 6)))
         #+SPHINX-SEARCH-DEBUG (format t "set-server -> ~s~%" path)
         (setf (%path client) path)
         (setf (%host client) ())
         (setf (%port client) ()))
        (t
         #+SPHINX-SEARCH-DEBUG (format t "set-server -> ~s : ~s~%" host port)
         (assert (stringp host))
         (assert (numberp port))
         (setf (%host client) host)
         (setf (%port client) port)
         (setf (%path client) ())))
  client)


(defgeneric set-limits (client &key offset limit max cutoff)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[offset]{the offset to start returning matches from}
    @arg[limit]{how many matches to return starting from @code{offset}}
    @arg[max]{maximum number of matches to return}
    @arg[cutoff]{the cutoff to stop searching at}
    @return{client}
    @short{Set the offset, limit, cutoff and max matches to return.}

    @begin{pre}
    (set-limits client :limit limit)
    (set-limits client :offset offset :limit limit)
    (set-limits client :offset offset :limit limit :max max-matches)
    @end{pre}

    Set limit of matches to return. Defaults to offset 0 and 1000 max matches.
"))

(defmethod set-limits ((client sphinx-client) &key (offset 0) limit (max 1000) cutoff)
  (assert (and (numberp offset) (numberp limit) (>= offset 0) (>= limit 0)))
  (assert (and (numberp max) (>= max 0)))
  (setf (offset client) offset)
  (setf (limit client) limit)
  (when (> max 0)
    (setf (max-matches client) max))
  (when (and cutoff (>= cutoff 0))
    (setf (cutoff client) cutoff))
  client)

(defgeneric set-id-range (client min max)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[min]{minimum id to start searching from}
    @arg[max]{maximum id to stop searching at}
    @return{client}
    @short{Set the id-range to search within (inclusive).}

    Set the range of id's within which to search. Range is inclusive, so setting
    [0, 450] both 0 and 450 id's will be found.
"))

(defmethod set-id-range ((client sphinx-client) min max)
  (assert (and (numberp min) (numberp max)
               (>= max min)))
  (setf (min-id client) min)
  (setf (max-id client) max))


(defgeneric set-filter (client attribute values-list &key exclude)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[attribute]{the attribute to filter on}
    @arg[values-list]{the numeric values to filter on}
    @arg[exclude]{if set, exclude the given values}
    @return{client}
    @short{Sets the results to be filtered on the given attribute.}

    @begin{pre}
    (set-filter client \"filter_attr\" '(0 2 4 34 55 77))
    (set-filter client \"other_attr\" '(8 4 2 11) :exclude t)
    @end{pre}

    Sets the results to be filtered on the given attribute. Only
    results which have attributes matching the given (numeric)
    values will be returned.

    This may be called multiple times with different attributes to
    select on multiple attributes.

    If @code{:exclude} is set, excludes results that match the filter.


    @see{set-filter}
    @see{set-filter-range}
    @see{set-filter-float-range}
    @see{set-geo-anchor}
    @see{reset-filters}
"))

(defmethod set-filter ((client sphinx-client) attr values &key (exclude ()))
  (assert (and (listp values) (> (length values) 0)))
  (dolist (item values)
    (assert (numberp item)))
  (push `(,+sph-filter-values+ ,attr ,values ,(cond (exclude 1) (t 0))) (filters client))
  client)


(defgeneric set-filter-range (client attribute min max &key exclude)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[attribute]{the attribute to filter on}
    @arg[min]{start of the range to filter on}
    @arg[max]{end of the range to filter on}
    @arg[exclude]{if set, exclude the given range}
    @return{client}
    @short{Sets the results to be filtered on the given range.}

    @begin{pre}
    (set-filter-range client \"filter_attr\" 45 99)
    (set-filter-range client \"other_attr\" 2 8 :exclude t)
    @end{pre}

    Sets the results to be filtered on a range of values for the given
    attribute. Only those records where the attribute value is between
    @code{min} and @code{max} (including @code{min} and @code{max})
    will be returned.

    This may be called multiple times with different attributes to
    select on multiple attributes.

    If @code{:exclude} is set, excludes results that fall within the
    given range.


    @see{set-filter}
    @see{set-filter-range}
    @see{set-filter-float-range}
    @see{set-geo-anchor}
    @see{reset-filters}
"))

(defmethod set-filter-range ((client sphinx-client) attr min max &key (exclude ()))
  (%set-filter-range client +sph-filter-range+ attr min max :exclude exclude))


(defgeneric set-filter-float-range (client attribute min max &key exclude)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[attribute]{the attribute to filter on}
    @arg[min]{start of the range to filter on}
    @arg[max]{end of the range to filter on}
    @arg[exclude]{if set, exclude the given range}
    @return{client}
    @short{Sets the results to be filtered on the given range.}

    @begin{pre}
    (set-filter-float-range client \"filter_attr\" 45.231 99)
    (set-filter-float-range client \"other_attr\" 1.32 55.0031 :exclude t)
    @end{pre}

    Sets the results to be filtered on a range of values for the given
    attribute. Only those records where the attribute value is between
    @code{min} and @code{max} (including @code{min} and @code{max})
    will be returned.

    This may be called multiple times with different attributes to
    select on multiple attributes.

    If @code{:exclude} is set, excludes results that fall within the
    given range.


    @see{set-filter}
    @see{set-filter-range}
    @see{set-filter-float-range}
    @see{set-geo-anchor}
    @see{reset-filters}
"))

(defmethod set-filter-float-range ((client sphinx-client) attr min max &key (exclude ()))
  (%set-filter-range client +sph-filter-floatrange+ attr min max :exclude exclude))

(defmethod %set-filter-range ((client sphinx-client) type attr min max &key (exclude ()))
  (assert (and (numberp min) (numberp max) (>= max min)))
  (push `(,type ,attr ,min ,max ,(cond (exclude 1) (t 0))) (filters client))
  client)


(defgeneric set-geo-anchor (client latitude-attribute latitude longitude-attribute longitude)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[latitude-attribute]{the latitude attribute name}
    @arg[latitude]{latitude in radians}
    @arg[longitude-attribute]{the longitude attribute name}
    @arg[longitude]{longitude in radians}
    @return{client}
    @short{Setup anchor point for geolocation.}

    @begin{pre}
    (set-geo-anchor client \"latitude_attr\" 45.231 \"longitude_attribute\" 4.5)
    @end{pre}

    Setup anchor point for using geosphere distance calculations in
    filters and sorting. Distance will be computed with respect to
    this point, and will be included in result output.

    To actually use this to filter on results a certain distance from
    the anchor point, use something like:

    @begin{pre}
    (set-filter-float-range sph \"@@geodist\" 0 5000)
    @end{pre}

    This will filter the results to be closer than 5 km from the anchor
    point.


    @see{set-filter}
    @see{set-filter-range}
    @see{set-filter-float-range}
    @see{set-geo-anchor}
    @see{reset-filters}
"))

(defmethod set-geo-anchor ((client sphinx-client) lat-attr lat lon-attr lon)
  (assert (and (stringp lat-attr) (stringp lon-attr) (numberp lat) (numberp lon)))
  (setf (geo-anchor client) (list lat-attr lat lon-attr lon))
  client)


(defgeneric set-group-by (client attribute function &optional group-sort)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[attribute]{the attribute name to group by}
    @arg[function]{the grouping function to use}
    @arg[group-sort]{the sorting clause for group-by}
    @return{client}
    @short{Set grouping options.}

    @see{set-group-by}
    @see{set-group-distinct}

    @begin{pre}
    (set-group-by client \"whatever_attr\" +sph-groupby-attr+ \"group asc\")
    (set-group-by client \"date_attr\" +sph-groupby-day+)
    @end{pre}

    Sets attribute and function of results grouping.

    In grouping mode, all matches are assigned to different groups based on
    grouping function value. Each group keeps track of the total match
    count, and the best match (in this group) according to current sorting
    function. The final result set contains one best match per group, with
    grouping function value and matches count attached.

    @code{attribute} is any valid attribute. Use @fun{reset-group-by}
    to disable grouping.

    @code{function} is one of:

    @begin{dl}
      @dt[+sph-groupby-day+]{Group by day (assumes timestamp type attribute
      of form YYYYMMDD)}
      @dt[+sph-groupby-week+]{Group by week (assumes timestamp type attribute
      of form YYYYNNN)}
      @dt[+sph-groupby-month+]{Group by month (assumes timestamp type
      attribute of form YYYYMM)}
      @dt[+sph-groupby-year+]{Group by year (assumes timestamp type attribute
      of form YYYY)}
      @dt[+sph-groupby-attr+]{Group by attribute value}
      @dt[+sph-groupby-attrpair+]{Group by two attributes, being the given
      attribute and the attribute that immediately follows it in the sequence
      of indexed attributes. The specified attribute may therefore not be the
      last of the indexed attributes}
    @end{dl}

    Groups in the set of results can be sorted by any SQL-like sorting clause,
    including both document attributes and the following special internal
    Sphinx attributes:

    @begin{dl}
      @dt[@id]{document ID}
      @dt[@weight, @rank, @relevance]{match weight}
      @dt[@group]{group by function value}
      @dt[@count]{number of matches in group}
    @end{dl}

    The default mode is to sort by group-by value in descending order,
    ie. by \"@@group desc\".

    In the results set, @code{total-found} contains the total amount of
    matching groups over the whole index.

    WARNING: grouping is done in fixed memory and thus its results
    are only approximate; so there might be more groups reported
    in @code{total-found} than actually present. @code{count} might
    also be underestimated.

    For example, if sorting by relevance and grouping by a \"published\"
    attribute with +sph-groupby-day+ function, then the result set will
    contain only the most relevant match for each day when there were any
    matches published, with day number and per-day match count attached,
    and sorted by day number in descending order (ie. recent days first).
"))

(defmethod set-group-by ((client sphinx-client) attr func &optional sort)
  (assert (and (stringp attr) (stringp sort) (find func +sph-sort-functions+)))
  (setf (group-by client) attr)
  (setf (group-function client) func)
  (setf (group-sort client) sort)
  client)


(defgeneric set-group-distinct (client attribute)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[attribute]{the attribute to use for count-distinct queries}
    @return{client}
    @short{Set count-distinct attribute for group-by queries.}


    @see{set-group-by}
    @see{set-group-distinct}
    @see{reset-group-by}
"))

(defmethod set-group-distinct ((client sphinx-client) attribute)
  (assert (stringp attribute))
  (setf (group-distinct client) attribute)
  client)


(defgeneric set-override (client attribute type values)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[attribute]{the attribute to override}
    @arg[type]{the attribute type as defined in Sphinx config}
    @arg[values]{an alist mapping document IDs to attribute values}
    @return{client}
    @short{Set attribute values overrides.}

    There can be only one override per attribute.

    @code{values} must be an alist that maps document IDs to attribute
    values.

    @begin{pre}
    (set-override client \"test_attr\" +sph-attr-integer+ '((4314 . 3) (2443 . 2)))
    @end{pre}

    In the example above, for the document with ID 4314, Sphinx will see an
    attribute value for the @code{attribute} called 'test_attr' of 3. And
    for the document with ID 2443 it will see 2, while the rest will be what
    it was when the indexer was last run.
"))

(defmethod set-override ((client sphinx-client) attribute type values)
  (assert (and (stringp attribute) (find type +sph-attr-types+) (listp values)))
  (push (cons attribute values) (overrides client))
  client)


(defgeneric set-select (client select)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[select]{the select string}
    @return{client}
    @short{Set the select clause.}

    Sets the select clause, listing specific attributes to fetch, and
    expressions to compute and fetch. Clause syntax mimics SQL.

    The select clause is very similar to the part of a typical SQL query
    between @code{SELECT} and @code{FROM}. It lets you choose what
    attributes (columns) to fetch, and also what expressions over the
    columns to compute and fetch. A difference from SQL is that expressions
    must always be aliased to a correct identifier (consisting of letters
    and digits) using the 'AS' keyword. Sphinx enforces aliases so that the
    computation results can be returned under a 'normal' name in the result
    set, used in other clauses, etc.

    Everything else is basically identical to SQL. Star ('*') is supported.
    Functions are supported. Arbitrary amount of expressions is supported.
    Computed expressions can be used for sorting, filtering, and grouping,
    just as the regular attributes.

    Aggregate functions (AVG(), MIN(), MAX(), SUM()) are supported when
    using GROUP BY.

    Examples:

    @begin{pre}
      (set-select sph \"*, (user_karma+ln(pageviews))*0.1 AS myweight\" )
      (set-select sph \"exp_years, salary_gbp*{$gbp_usd_rate@} AS salary_usd, IF(age>40,1,0) AS over40\" )
      (set-select sph \"*, AVG(price) AS avgprice\" )
    @end{pre}
"))

(defmethod set-select ((client sphinx-client) select)
  (assert (stringp select))
  (setf (select client) select)
  client)


(defgeneric reset-filters (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{client}
    @short{Reset the filters.}

    Clear all filters, including the geolocation anchor point.


    @see{set-filter}
    @see{set-filter-range}
    @see{set-filter-float-range}
    @see{set-geo-anchor}
    @see{reset-filters}
"))

(defmethod reset-filters ((client sphinx-client))
  (setf (filters client) ())
  (setf (geo-anchor client) ())
  client)


(defgeneric reset-group-by (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{client}
    @short{Clear all the group-by settings.}


    @see{set-group-by}
    @see{set-group-distinct}
    @see{reset-group-by}
"))

(defmethod reset-group-by ((client sphinx-client))
  (setf (group-by client) "")
  (setf (group-function client) +sph-groupby-day+)
  (setf (group-sort client) "@group desc")
  (setf (group-distinct client) "")
  client)


(defgeneric reset-overrides (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{client}
    @short{Clear all attribute value overrides.}
"))

(defmethod reset-overrides ((client sphinx-client))
  (setf (overrides client) ())
  client)


(defgeneric run-query (client query &key index comment)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[query]{the query to run through @code{searchd}}
    @arg[index]{the index to use; defaults to \"*\"}
    @arg[comment]{a comment describing this query; default none}
    @return{nil or a hash containing the query results}
    @short{Run a query through @code{searchd}.}

    @begin{pre}
    (run-query client \"test\")
    @end{pre}

    Query @code{searchd}. This method runs a single query through @code{searchd}.

    It returns the results in a hash with the following keys:
    @begin{dl}
      @dt[attributes]{a hash-table containing attributes}
      @dt[fields]{a list of fields}
      @dt[matches]{a hash-table containing the matches}
      @dt[status]{the status returned by @code{searchd}}
      @dt[status-message]{the status message returned by @code{searchd}}
      @dt[time]{the time @code{searchd} took for the query}
      @dt[total]{the total matches returned}
      @dt[total-found]{the total number of matches found}
      @dt[words]{a hash-table containing the matching words with their statistics}
    @end{dl}

    @see{add-query}
    @see{run-queries}

"))

(defmethod run-query ((client sphinx-client) query &key (index "*") (comment ""))
  (assert (eql (length (reqs client)) 0))
  (add-query client query :index index :comment comment)
  (let* ((result (car (run-queries client))))
    (when result
      (setf (last-error client) (gethash 'status-message result))
      (setf (last-warning client) (gethash 'status-message result))
      (let ((status (gethash 'status result)))
        (setf (status client) status)
        (when (or (eql status +searchd-ok+)
                  (eql status +searchd-warning+))
          result)))))


(defgeneric run-queries (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{nil or a list of hashes}
    @short{Run the queries added with @code{add-query} through @code{searchd}.}

    @begin{pre}
    (add-query client \"test\")
    (add-query client \"word\")
    (run-queries client)
    @end{pre}

    Query @code{searchd} with the collected queries added with @code{add-query}.

    It returns a list of hashes containing the result of each query. Each hash
    has the following keys:
    @begin{dl}
      @dt[attributes]{a hash-table containing attributes}
      @dt[fields]{a list of fields}
      @dt[matches]{a hash-table containing the matches}
      @dt[status]{the status returned by @code{searchd}}
      @dt[status-message]{the status message returned by @code{searchd}}
      @dt[time]{the time @code{searchd} took for the query}
      @dt[total]{the total matches returned}
      @dt[total-found]{the total number of matches found}
      @dt[words]{a hash-table containing the matching words with their statistics}
    @end{dl}

    @see{run-query}
    @see{add-query}

"))

(defmethod run-queries ((client sphinx-client))
  (assert (> (length (reqs client)) 0))
  (let ((requests (pack "Na*" (length (reqs client)) (reqs client))))
    #+SPHINX-SEARCH-DEBUG (format t "requests:~%~A~%length requests: ~a~%" requests (length requests))
    (let ((data (pack "nnN/a*" +searchd-command-search+ +ver-command-search+ requests)))
      (setf (reqs client) ())
      (when (%connect client)
        (%send client data)
        (let ((response (%get-response client :client-version +ver-command-search+)))
          #+SPHINX-SEARCH-DEBUG (format t "run-queries response: ~a~%" response)
          (when response
            (setf *response-length* (length response))
            (%parse-response response (length (reqs client)))))))))


(defgeneric add-query (client query &key index comment)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[query]{the query to run through @code{searchd}}
    @arg[index]{the index to use; defaults to \"*\"}
    @arg[comment]{a comment describing this query; default none}
    @return{length of query queue}
    @short{Add a query to a batch request.}

    @begin{pre}
    (add-query client \"test\")
    (add-query client \"word\" :index \"*\")
    (run-queries client)
    @end{pre}

    Add a query to the queue of batched queries.

    Batch queries enable @code{searchd} to perform internal optimizations,
    if possible; and reduce network connection overhead in all cases.

    For instance, running exactly the same query with different
    group-by settings will enable @code{searchd} to perform expensive
    full-text search and ranking operation only once, but compute
    multiple group-by results from its output.

    It returns the new length of the query queue, which is also the index
    of the newly added query in the queue.

    @see{run-query}
    @see{run-queries}

"))

(defmethod add-query ((client sphinx-client) query &key (index "*") (comment ""))
  (let ((req (concatenate 'string
                          (pack "NNNNN" (offset client) (limit client) (match-mode client) (rank-mode client) (sort-mode client))
                          (pack "N/a*" (sort-by client))
                          (pack "N/a*" (octets-to-string (string-to-octets query :encoding (%encoding client)) :encoding :latin-1))
                          (pack "N*" (length (weights client)) (weights client))
                          (pack "N/a*" index)
                          (pack "N" 1) (pack "Q>" (min-id client)) (pack "Q>" (max-id client))
                          (pack "N" (length (filters client)))
                          (%pack-filters (filters client))
                          (pack "NN/a*" (group-function client) (group-by client))
                          (pack "N" (max-matches client))
                          (pack "N/a*" (group-sort client))
                          (pack "NNN" (cutoff client) (retry-count client) (retry-delay client))
                          (pack "N/a*" (group-distinct client))
                          (cond ((geo-anchor client)
                                 (concatenate 'string
                                              (pack "N/a*" (first (geo-anchor client)))
                                              (pack "N/a*" (third (geo-anchor client)))
                                              (%pack-float (second (geo-anchor client)))
                                              (%pack-float (fourth (geo-anchor client)))))
                                (t
                                 (pack "N" 0)))
                          (%pack-hash (index-weights client))
                          (pack "N" (max-query-time client))
                          (%pack-hash (field-weights client))
                          (pack "N/a*" comment)
                          (pack "N" (hash-table-count (overrides client)))
                          (%pack-overrides (overrides client))
                          (pack "N/a*" (if (select client)
                                           (select client)
                                           "")))))
    #+SPHINX-SEARCH-DEBUG (format t "req is: ~a~%" (string-to-octets req :encoding (%encoding client)))
    (setf (reqs client) (append (reqs client) (list req))))
  (length (reqs client)))


(defmethod %connect ((client sphinx-client))
  #+SPHINX-SEARCH-DEBUG (format t "socket is: ~a~%" (%socket client))
  (cond ((%socket client))
        ((%path client)
         (setf (%socket client)
               (sockets:make-socket :address-family :local :type :stream
                                    :local-filename (namestring (%path client)))))
        (t
         (setf (%socket client)
               (sockets:make-socket :address-family :internet :type :stream
                                    :remote-host (%host client)
                                    :remote-port (%port client)))))
  (let ((v (unpack "N*" (%read-from client 4))))
    (if (< v 1)
        (progn
          (close (%socket client))
          (setf (last-error client) "connection to socket failed")
          ())
        (progn
          (sockets:send-to (%socket client)
                           (string-to-octets (pack "N" 1) :encoding :latin-1))
          #+SPHINX-SEARCH-DEBUG (format t "recieved version number: ~a~%" v)
          (%socket client)))))


(defmethod %read-from ((client sphinx-client) size)
  (let ((rec (sockets:receive-from (%socket client) :size size)))
    #+SPHINX-SEARCH-DEBUG (format t "recieved bytes: ~a~%" rec)
    (let ((res
           (octets-to-string (coerce rec '(vector (unsigned-byte 8)))
                             :encoding :latin-1)))
      #+SPHINX-SEARCH-DEBUG (format t "octets-to-string gives: ~a~%" res)
      res)))


(defmethod %get-response ((client sphinx-client) &key client-version)
  (multiple-value-bind (status version len) (unpack "n2N" (%read-from client 8))
    #+SPHINX-SEARCH-DEBUG (format t "status: ~a~%version: ~a~%length: ~a~%" status version len)
    (let ((response ())
          (left len))
      (loop
         (when (<= left 0)
           (return))
         #+SPHINX-SEARCH-DEBUG (format t "left: ~a~%" left)
         (let ((chunk (%read-from client left)))
           #+SPHINX-SEARCH-DEBUG (format t "chunk: ~a~%" chunk)
           #+SPHINX-SEARCH-DEBUG (format t "chunk length: ~a~%" (length chunk))
           (if (> (length chunk) 0)
               (progn
                 (setf response (concatenate 'string response chunk))
                 (setf left (- left (length chunk))))
               (return))))
      (close (%socket client))
      (setf (%socket client) ())
      (let ((done (length response)))
        #+SPHINX-SEARCH-DEBUG (format t "got response of length: ~a~%raw response: ~a~%" done response)
        (cond ((or (not response)
                   (not (eql done len)))
               (if len
                   (setf (last-error client) "failed to read searchd response (status=x, ver=x, len=x, read=x)")
                   (setf (last-error client) "received zero-sized searchd response"))
               '())
              ((eql status +searchd-warning+)
               (let ((warn-length (+ 4 (unpack "N" (subseq response 0 4)))))
                 (setf (last-warning client) (subseq response 4 (+ 4 warn-length)))
                 (subseq response (+ 4 warn-length))))
              ((eql status +searchd-error+)
               (setf (last-error client) (subseq response 4))
               '())
              ((eql status +searchd-retry+)
               (setf (last-error client) (subseq response 4))
               '())
              ((not (eql status +searchd-ok+))
               (setf (last-error client) "unknown status code: x")
               '())
              (t
               (when (< version client-version)
                 (setf (last-warning client) "searchd v.x.x is older than client's v.y.y, some options might not work"))
               response))))))


(defun %parse-response (response n-requests)
  (let ((p 0)
        (results ()))
    (loop for i from 0 to n-requests
       do
       (multiple-value-bind (status new-p message) (%get-response-status response p)
         (let ((result (make-hash-table)))
           (setf p new-p)
           (setf (gethash 'status-message result) message)
           (setf (gethash 'status result) status)
           (when (or (eql status +searchd-ok+)
                     (eql status +searchd-warning+))
             (let ((attribute-names ()))
               (multiple-value-bind (fields new-p) (%get-fields response p)
                 (setf p new-p)
                 (setf (gethash 'fields result) fields))
               #+SPHINX-SEARCH-DEBUG (format t "after get-fields:~%  p: ~a~%  rest: ~a~%" p (subseq response p))
               (multiple-value-bind (attributes attr-names new-p) (%get-attributes response p)
                 (setf p new-p)
                 (setf (gethash 'attributes result) attributes)
                 (setf attribute-names attr-names))
               #+SPHINX-SEARCH-DEBUG (format t "after get-attributes:~%  p: ~a~%  rest: ~a~%" p (subseq response p))
               (multiple-value-bind (matches new-p) (%get-matches response attribute-names (gethash 'attributes result) p)
                 (setf p new-p)
                 (setf (gethash 'matches result) matches))
               #+SPHINX-SEARCH-DEBUG (format t "after get-matches:~%  p: ~a~%  rest: ~a~%" p (subseq response p))
               (multiple-value-bind (total total-found time word-count) (unpack "N*N*N*N*" (subseq response p (+ p 16)))
                 (adv-p 16)
                 #+SPHINX-SEARCH-DEBUG (format t "total: ~a~%total-found: ~a~%time: ~a~%word-count: ~a~%" total total-found time word-count)
                 (setf (gethash 'total result) total)
                 (setf (gethash 'total-found result) total-found)
                 (let ((time-str (with-output-to-string (s)
                                   (format s "~,8f" (/ time 1000)))))
                   (setf (gethash 'time result) time-str))
                 (let ((words (make-hash-table :test 'equal)))
                   (dotimes (n word-count)
                     (let* ((len (unpack "N*" (subseq response p (+ p 4))))
                            (word (subseq response (+ p 4) (+ p 4 len)))
                            (docs (unpack "N*" (subseq response (+ p 4 len) (+ p 4 len 4))))
                            (hits (unpack "N*" (subseq response (+ p 8 len) (+ p 8 len 4))))
                            (word-info (make-hash-table)))
                       #+SPHINX-SEARCH-DEBUG (format t "len: ~a~%p: ~a~%" *response-length* p)
                       #+SPHINX-SEARCH-DEBUG (format t "rest: '~a'~%" (subseq response p))
                       #+SPHINX-SEARCH-DEBUG (format t "subseq: ~a~%" (subseq response p (+ p 4)))
                       #+SPHINX-SEARCH-DEBUG (format t "len: ~a~%" len)
                       #+SPHINX-SEARCH-DEBUG (format t "subseq: ~a~%" (subseq response (+ p 4) (+ p 4 len)))
                       #+SPHINX-SEARCH-DEBUG (format t "word: ~a~%docs: ~a~%hits: ~a~%" word docs hits)
                       (adv-p (+ len 12))
                       (setf (gethash 'docs word-info) docs)
                       (setf (gethash 'hits word-info) hits)
                       (setf (gethash word words) word-info)
                       (when (> p *response-length*)
                         (return))))
                   (setf (gethash 'words result) words)))))
           (push result results))))
    results))


(defun %get-matches (response attribute-names attributes start)
  (let ((count (unpack "N*" (subseq response start (+ start 4))))
        (id-64 (unpack "N*" (subseq response (+ start 4) (+ start 4 4))))
        (p (+ start 8))
        (matches ()))
    #+SPHINX-SEARCH-DEBUG (format t "get-matches:~%  start: ~a~%  rest: ~a~%" start (subseq response start))
    #+SPHINX-SEARCH-DEBUG (format t "  count: ~a~%  id-64: ~a~%" count id-64)
    (dotimes (i count)
      (let ((data (make-hash-table :test 'equal)))
        (cond ((not (eql id-64 0))
               (setf (gethash "doc" data) (unpack "Q>" (subseq response p (+ p 8))))
               (adv-p 8)
               (setf (gethash "weight" data) (unpack "N*" (subseq response p (+ p 4))))
               (adv-p 4))
              (t
               (setf (gethash "doc" data) (unpack "N*" (subseq response p (+ p 4))))
               (adv-p 4)
               (setf (gethash "weight" data) (unpack "N*" (subseq response p (+ p 4))))
               (adv-p 4)))
        #+SPHINX-SEARCH-DEBUG (format t "  -> doc: ~a~%  -> weight: ~a~%" (gethash "doc" data) (gethash "weight" data))
        (dolist (attr attribute-names)
          (cond ((eql (gethash attr attributes) +sph-attr-bigint+)
                 #+SPHINX-SEARCH-DEBUG (format t "  -> attribute '~a' is bigint~%" attr)
                 (setf (gethash attr data) (unpack "q>" (subseq response p (+ p 8))))
                 (adv-p 8))
                ((eql (gethash attr attributes) +sph-attr-float+)
                 #+SPHINX-SEARCH-DEBUG (format t "  -> attribute '~a' is float~%" attr)
                 (let* ((uval (unpack "N*" (subseq response p (+ p 4))))
                        (tmp (pack "L" uval))
                        (floats (multiple-value-list (unpack "f*" tmp))))
                   (adv-p 4)
                   (setf (gethash attr data) floats)))
                (t
                 (let ((val (unpack "N*" (subseq response p (+ p 4)))))
                   (adv-p 4)
                   #+SPHINX-SEARCH-DEBUG (format t "  -> attr '~a': val: ~a~%" attr val)
                   (cond ((not (eql (logand +sph-attr-multi+ (gethash attr attributes)) 0))
                          #+SPHINX-SEARCH-DEBUG (format t "  -> attribute '~a' is multival~%" attr)
                          (let ((vals ()))
                            (dotimes (i val)
                              (push (unpack "N*" (subseq response p (+ p 4))) vals)
                              (adv-p 4)
                              (when (> p *response-length*)
                                (return)))
                            #+SPHINX-SEARCH-DEBUG (format t "  -> vals: ~a~%" vals)
                            (setf (gethash attr data) (nreverse vals))))
                         (t
                          #+SPHINX-SEARCH-DEBUG (format t "  -> attribute '~a' is other: val = ~a~%" attr val)
                          (setf (gethash attr data) val)))))))
        (push data matches)))
    #+SPHINX-SEARCH-DEBUG (format t "  -> matches: ~a~%" matches)
    (values (nreverse matches) p)))


(defun %get-attributes (response start)
  (let ((nattrs (unpack "N*" (subseq response start (+ start 4))))
        (p (+ start 4))
        (attribute-names ())
        (attributes (make-hash-table :test 'equal)))
    #+SPHINX-SEARCH-DEBUG (format t "get-attributes:~%  nattrs: ~a~%" nattrs)
    (dotimes (i nattrs)
      (let ((len (unpack "N*" (subseq response p (+ p 4)))))
        #+SPHINX-SEARCH-DEBUG (format t "  attr: ~a~%  -> len: ~a~%" i len)
        (adv-p 4)
        (let ((attr-name (subseq response p (+ p len))))
          #+SPHINX-SEARCH-DEBUG (format t "  -> attr-name subseq: ~a~%" (subseq response p (+ p len)))
          #+SPHINX-SEARCH-DEBUG (format t "  -> attr-name: ~a~%" attr-name)
          (adv-p len)
          (setf (gethash attr-name attributes) (unpack "N*" (subseq response p (+ p 4))))
          #+SPHINX-SEARCH-DEBUG (format t "  -> attributes{~a}: ~a~%" attr-name (gethash attr-name attributes))
          (adv-p 4)
          (push attr-name attribute-names)
          (when (> p *response-length*)
            (return)))))
    #+SPHINX-SEARCH-DEBUG (format t "  attribute-names: ~a~%"  attribute-names)
    (values attributes (nreverse attribute-names) p)))


(defun %get-fields (response start)
  (let ((nfields (unpack "N" (subseq response start (+ start 4))))
        (p (+ start 4))
        (fields ()))
    #+SPHINX-SEARCH-DEBUG (format t "get-fields:~%")
    #+SPHINX-SEARCH-DEBUG (format t "  subseq starting at ~a: '~a'~%" start (subseq response start (+ start 4)))
    #+SPHINX-SEARCH-DEBUG (format t "  start: ~a~%  nfields: ~a~%  p: ~a~%" start nfields p)
    (dotimes (i nfields)
      (let ((len (unpack "N" (subseq response p (+ p 4)))))
        #+SPHINX-SEARCH-DEBUG (format t "i: ~a~%  len: ~a~%" i len)
        (adv-p 4)
        (push (subseq response p (+ p len)) fields)
        (adv-p len)
        (when (> p *response-length*)
          (return))))
    #+SPHINX-SEARCH-DEBUG (format t "  fields: ~a~%" fields)
    (values (nreverse fields) p)))


(defun %get-response-status (response start)
  (let ((status (unpack "N" (subseq response start (+ start 4))))
        (p (+ start 4)))
    (cond ((not (eql status +searchd-ok+))
           (let ((len (unpack "N" (subseq response p (+ p 4)))))
             (setf p (+ p 4))
             (let ((message (subseq response p (+ p len))))
               (values status (+ p len) message))))
          (t
           (values status p "ok")))))


(defmethod %send ((client sphinx-client) data)
  #+SPHINX-SEARCH-DEBUG (format t "writing to socket ~a~%" (%socket client))
  #+SPHINX-SEARCH-DEBUG (format t "data to be sent: ~a~%" data)
  #+SPHINX-SEARCH-DEBUG (format t "data as octets: ~a~%" (string-to-octets data :encoding :latin-1))
  (sockets:send-to (%socket client) (string-to-octets data :encoding :latin-1)))


(defun %pack-overrides (overrides)
  (when (hash-table-p overrides)
    (maphash #'(lambda (k entry)
                 (declare (ignore k))
                 (concatenate 'string
                              (pack "N/a*" (gethash 'attr entry))
                              (pack "NN" (gethash 'type entry) (hash-table-count (gethash 'values entry)))
                              (maphash #'(lambda (id v)
                                           (concatenate 'string
                                                        (assert (and (numberp id) (numberp v)))
                                                        (pack "Q>" id)
                                                        (cond ((eql (gethash 'type entry) +sph-attr-float+)
                                                               (%pack-float v))
                                                              ((eql (gethash 'type entry) +sph-attr-bigint+)
                                                               (pack "q>" v))
                                                              (t
                                                               (pack "N" v)))))
                                       (gethash 'values entry))))
             overrides)))


(defun %pack-filters (filters)
  (with-output-to-string (packed-filters)
    (dolist (filter filters)
      (let ((type (first filter))
            (attr (second filter))
            (last-el 3))
        (format packed-filters "~a~a~a~a"
                (pack "N/a*" attr)
                (pack "N" type)
                (cond ((eql type +sph-filter-values+)
                       (%pack-list-signed-quads (third filter)))
                      ((eql type +sph-filter-range+)
                       (concatenate 'string
                                    (pack "q>" (third filter))
                                    (pack "q>" (fourth filter)))
                       (incf last-el))
                      ((eql type +sph-filter-floatrange+)
                       (concatenate 'string
                                    (%pack-float (third filter))
                                    (%pack-float (fourth filter)))
                       (incf last-el))
                      (t
                       (error "Unhandled filter type ~S" type)))
                (pack "N" (nth last-el filter)))))))


(defun %pack-hash (hash-table)
  (concatenate 'string
               (pack "N" (hash-table-count hash-table))
               (when (hash-table-count hash-table)
                 (maphash #'(lambda (k v)
                              (pack "N/a*N" k v))
                          hash-table))))


(defun %pack-list-signed-quads (values-list)
  (with-output-to-string (packed-list)
    (format packed-list "~a" (pack "N" (length values-list)))
    (dolist (value values-list)
      (format packed-list "~a" (pack "q>" value)))))


(defun %pack-float (float-value)
  (pack "N" (unpack "L*" (pack "f" float-value))))

