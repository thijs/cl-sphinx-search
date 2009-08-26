;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:com.oppermannen.sphinx-search-api)


(defclass sphinx-client ()
  ((sphinx-host
    :accessor sphinx-host
    :initarg :host
    :initform "localhost"
    :documentation "searchd host (default is 'localhost')")
   (sphinx-port
    :accessor sphinx-port
    :initarg :port
    :initform 3312
    :documentation "searchd port (default is 3312)")
   (sphinx-path
    :accessor sphinx-path
    :initarg :path
    :initform ()
    :documentation "searchd unix-domain socket path")
   (sphinx-socket
    :accessor sphinx-socket
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
    :documentation "requests array for multi-query")))



(defmethod set-server ((client sphinx-client) &key host port)
  (format t "~s : ~s" host port)
  (assert (stringp host))
  (cond ((string= host "/" :start1 0 :end1 1)
         (setf (sphinx-path client) host)
         (setf (sphinx-host client) ())
         (setf (sphinx-port client) ()))
        ((string= host "unix://" :start1 0 :end1 7)
         (setf (sphinx-path client) (subseq host 6 (length host)))
         (setf (sphinx-host client) ())
         (setf (sphinx-port client) ()))
        (t
         (format t "~s : ~s" host port)
         (assert (numberp port))
         (setf (sphinx-host client) host)
         (setf (sphinx-port client) port)
         (setf (sphinx-path client) ()))))


(defmethod %connect ((client sphinx-client))
  (cond ((sphinx-socket client))
        ((sphinx-path client)
         (setf (sphinx-socket client)
               (sockets:make-socket :address-family :local :type :stream
                            :local-filename (namestring (sphinx-path client)))))
        (t
         (setf (sphinx-socket client)
               (sockets:make-socket :address-family :internet :type :stream
                                    :remote-host (sphinx-host client)
                                    :remote-port (sphinx-port client)))))
  (let ((v (unpack "N*" (sb-ext:octets-to-string
                         (coerce
                          (sockets:receive-from (sphinx-socket client) :size 4)
                          '(vector (unsigned-byte 8)))
                         :external-format :latin-1))))
    (if (< v 1)
        (progn
          (close (sphinx-socket client))
          (setf (last-error client) "connection to socket failed"))
        (progn
          (sockets:send-to (sphinx-socket client)
                           (sb-ext:string-to-octets (pack "N" 1) :external-format :latin-1))
          (format t "~a~%" v)
          (sphinx-socket client)))))

(defun read-from (socket size)
  (let ((rec (sockets:receive-from socket :size size)))
    (format t "~a~%" rec)
    (let ((res
           (sb-ext:octets-to-string
            (coerce rec
                    '(vector (unsigned-byte 8)))
            :external-format :latin-1)))
      (format t "res: ~a~%" res)
      res)))

(defmethod %get-response ((client sphinx-client) &key client-version)
  (multiple-value-bind (status version len) (unpack "n2N" (read-from (sphinx-socket client) 8))
    (format t "~a : ~a : ~a~%" status version len)
    (let ((response ())
          (left len))
      (loop
         (when (< left 0)
           (return))
         (let ((chunk (read-from (sphinx-socket client) left)))
           (if (> (length chunk) 0)
               (progn
                 (setf response (concatenate 'vector response chunk))
                 (- left (length chunk)))
               (return))))
      (let ((done (length response)))
        (cond ((or (not response)
                   (not (eql done len)))
               (if len
                   (setf (last-error client) "failed to read searchd response (status=x, ver=x, len=x, read=x)")
                   (setf (last-error client) "received zero-sized searchd response"))
               '())
              ((eql status +searchd-warning+)
               (let ((warn-length (+ 4 (unpack "N" (subseq response 0 4)))))
                 (setf (last-warning client) (subseq response 4 warn-length))
                 (subseq response warn-length)))
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




