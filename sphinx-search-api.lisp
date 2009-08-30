;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:com.oppermannen.sphinx-search-api)


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
   (reqs
    :accessor reqs
    :initarg :reqs
    :initform ()
    :documentation "requests array for multi-query")))



(defmethod set-server ((client sphinx-client) &key host port)
  (format t "~s : ~s" host port)
  (assert (stringp host))
  (cond ((string= host "/" :start1 0 :end1 1)
         (setf (%path client) host)
         (setf (%host client) ())
         (setf (%port client) ()))
        ((string= host "unix://" :start1 0 :end1 7)
         (setf (%path client) (subseq host 6 (length host)))
         (setf (%host client) ())
         (setf (%port client) ()))
        (t
         (format t "~s : ~s" host port)
         (assert (numberp port))
         (setf (%host client) host)
         (setf (%port client) port)
         (setf (%path client) ()))))


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
  (let ((v (unpack "N*" (read-from (%socket client) 4))))
    (if (< v 1)
        (progn
          (close (%socket client))
          (setf (last-error client) "connection to socket failed"))
        (progn
          (sockets:send-to (%socket client)
                           (string-to-octets (pack "N" 1) :encoding :utf-8))
          (format t "~a~%" v)
          (%socket client)))))

(defun read-from (socket size)
  (let ((rec (sockets:receive-from socket :size size)))
    (format t "~a~%" rec)
    (let ((res
           (octets-to-string
            (coerce rec
                    '(vector (unsigned-byte 8)))
            :encoding :utf-8)))
      (format t "res: ~a~%" res)
      res)))

(defmethod %get-response ((client sphinx-client) &key fp client-version)
  (multiple-value-bind (status version len) (unpack "n2N" (read-from fp 8))
    (format t "~a : ~a : ~a~%" status version len)
    (let ((response ())
          (left len))
      (loop
         (when (< left 0)
           (return))
         (let ((chunk (read-from fp left)))
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

(defmethod set-limits ((client sphinx-client) &key offset limit max cutoff)
  (assert (and (numberp offset) (numberp limit) (>= offset 0) (>= limit 0)))
  (assert (and (numberp max) (>= max 0)))
  (setf (offset client) offset)
  (setf (limit client) limit)
  (when (> max 0)
    (setf (max-matches client) max))
  (when (and cutoff (>= cutoff 0))
    (setf (cutoff client) cutoff)))


(defmethod run-queries ((client sphinx-client))
  (assert (> (length (reqs client)) 0))
  (let* ((requests (pack "Na*" (length (reqs client)) (reqs client)))
         (data (pack "nnN/a*" +searchd-command-search+ +ver-command-search+ requests)))
    (setf (reqs client) ())
    (let ((fp (%connect client)))
      (when fp
        (%send client :fp fp :data data)
        (let ((response (%get-response client :fp fp :client-version +ver-command-search+)))
          (format t "~a~%" response))))))


(defmethod %send ((client sphinx-client) &key fp data)
  (format t "Writing to socket ~a~%" fp)
  (sockets:send-to fp (string-to-octets data :encoding :utf-8)))


(defmethod add-query ((client sphinx-client) &key query (index "*") (comment ""))
 (let ((req (concatenate 'string
                          (pack "NNNNN" (offset client) (limit client) (mode client) (ranker client) (sort-mode client))
                          (pack "N/a*" (sort-by client))
                          (pack "N/a*" query)
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
   (format t "req is: ~a~%" req)
   (setf (reqs client) (append (reqs client) (list req))))
 (length (reqs client)))


(defun %pack-overrides (overrides)
  (when (hash-table-p overrides)
    (maphash #'(lambda (k entry)
                 (concatenate 'string
                              (pack "N/a*" (get-hash 'attr entry))
                              (pack "NN" (get-hash 'type entry) (hash-table-count (get-hash 'values entry)))
                              (maphash #'(lambda (id v)
                                           (concatenate 'string
                                                        (assert (and (numberp id) (numberp v)))
                                                        (pack "Q>" id)
                                                        (cond ((eql (get-hash 'type entry) +sph-attr-float+)
                                                               (%pack-float v))
                                                              ((eql (get-hash 'type entry) +sph-attr-bigint+)
                                                               (pack "q>" v))
                                                              (t
                                                               (pack "N" v)))))
                                       (get-hash 'values entry))))
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
                                                        (%pack-array-signed-quads (get-hash 'values filter)))
                                                       ((eql type +sph-filter-range+)
                                                        (concatenate 'string (pack "q>" (get-hash 'min filter))
                                                                     (pack "q>" (get-hash 'max filter))))
                                                       ((eql type +sph-filter-floatrange+)
                                                        (concatenate 'string (%pack-float (get-hash 'min filter))
                                                                     (%pack-float (get-hash 'max filter))))
                                                       (t
                                                        (error "Unhandled filter type ~S" type)))
                                                 (pack "N" (get-hash 'exclude filter)))))))
       filters))




(defun %pack-hash (hash-table)
  (when (hash-table-count hash-table)
  (concatenate 'string
               (pack "N" (hash-table-count hash-table))
               (maphash #'(lambda (k v)
                            (pack "N/a*N" k v))
                        hash-table))))


(defun %pack-array-signed-quads (values-list)
  (concatenate 'string
               (pack "N" (length values-list))
               (map #'(lambda (value)
                        (pack "q>" value)) values-list)))

(defun %pack-float (float-value)
  (pack "N" (unpack "L*" (pack "f" float-value))))



