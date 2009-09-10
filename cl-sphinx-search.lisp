;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-sphinx-search)


(defvar *response-length* ())


(defmacro adv-p (n)
  `(setf p (+ p ,n)))


(defgeneric last-error (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{a string; the last error message returned from the @code{searchd}}

    Get the last error message sent by searchd
"))


(defgeneric last-warning (client)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @return{a string; the last warning message returned from the @code{searchd}}

    Get the last warning message sent by searchd
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
    :documentation "search filters; a list of hashes")
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
   (anchor
    :accessor anchor
    :initarg :anchor
    :initform ()
    :documentation "geographical anchor point; fixed length list with '(attrlat lat attrlon lon)")
   (index-weights
    :accessor index-weights
    :initarg :index-weights
    :initform (make-hash-table)
    :documentation "per-index weights")
   (ranker
    :accessor ranker
    :initarg :ranker
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
    @fun{query}, or add a number of queries using @fun{add-query} and
    then calling @fun{run-queries}.

    Either get a result hash or a list of result hashes back, or an error
    that can be retrieved with the @fun{last-error} function.

    @see{set-server}
    @see{set-limits}
    @see{last-warning}
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


(defgeneric query (client query &key index comment)
  (:documentation
   "@arg[client]{a @class{sphinx-client}}
    @arg[query]{the query to run through @code{searchd}}
    @arg[index]{the index to use; defaults to \"*\"}
    @arg[comment]{a comment describing this query; default none}
    @return{nil or a hash containing the query results}
    @short{Run a query through @code{searchd}.}

    @begin{pre}
    (query client \"test\")
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

(defmethod query ((client sphinx-client) query &key (index "*") (comment ""))
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

    @see{query}
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

    @see{query}
    @see{run-queries}

"))

(defmethod add-query ((client sphinx-client) query &key (index "*") (comment ""))
  (let ((req (concatenate 'string
                          (pack "NNNNN" (offset client) (limit client) (mode client) (ranker client) (sort-mode client))
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
                          (cond ((anchor client)
                                 (concatenate 'string
                                              (pack "N/a*" (first (anchor client)))
                                              (pack "N/a*" (third (anchor client)))
                                              (%pack-float (second (anchor client)))
                                              (%pack-float (last (anchor client)))))
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
  (map 'string #'(lambda (filter)
                   (when (hash-table-p filter)
                     (concatenate 'string
                                  (pack "N/a*" (gethash 'attr filter))
                                  (let ((type (gethash 'type filter)))
                                    (concatenate 'string
                                                 (pack "N" type)
                                                 (cond ((eql type +sph-filter-values+)
                                                        (%pack-array-signed-quads (gethash 'values filter)))
                                                       ((eql type +sph-filter-range+)
                                                        (concatenate 'string (pack "q>" (gethash 'min filter))
                                                                     (pack "q>" (gethash 'max filter))))
                                                       ((eql type +sph-filter-floatrange+)
                                                        (concatenate 'string (%pack-float (gethash 'min filter))
                                                                     (%pack-float (gethash 'max filter))))
                                                       (t
                                                        (error "Unhandled filter type ~S" type)))
                                                 (pack "N" (gethash 'exclude filter)))))))
       filters))


(defun %pack-hash (hash-table)
  (concatenate 'string
               (pack "N" (hash-table-count hash-table))
               (when (hash-table-count hash-table)
                 (maphash #'(lambda (k v)
                              (pack "N/a*N" k v))
                          hash-table))))


(defun %pack-array-signed-quads (values-list)
  (concatenate 'string
               (pack "N" (length values-list))
               (map 'string #'(lambda (value)
                                (pack "q>" value)) values-list)))


(defun %pack-float (float-value)
  (pack "N" (unpack "L*" (pack "f" float-value))))


