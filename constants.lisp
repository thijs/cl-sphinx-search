;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:com.oppermannen.sphinx-search-api)


;; known searchd commands
(defconstant +searchd-command-search+ 0)
(defconstant +searchd-command-excerpt+ 1)
(defconstant +searchd-command-update+ 2)
(defconstant +searchd-command-keywords+ 3)
(defconstant +searchd-command-persist+ 4)

;; current client-side command implementation versions
(defconstant +ver-command-search+ #x116)
(defconstant +ver-command-excerpt+ #x100)
(defconstant +ver-command-update+ #x101)
(defconstant +ver-command-keywords+ #x100)

;; known searchd status codes
(defconstant +searchd-ok+ 0)
(defconstant +searchd-error+ 1)
(defconstant +searchd-retry+ 2)
(defconstant +searchd-warning+ 3)

;; known match modes
(defconstant +sph-match-all+ 0)
(defconstant +sph-match-any+ 1)
(defconstant +sph-match-phrase+ 2)
(defconstant +sph-match-boolean+ 3)
(defconstant +sph-match-extended+ 4)
(defconstant +sph-match-fullscan+ 5)
(defconstant +sph-match-extended2+ 6)

;; known ranking modes (extended2 mode only)
(defconstant +sph-rank-proximity-bm25+ 0) ;; default mode, phrase proximity major factor and BM25 minor one
(defconstant +sph-rank-bm25+ 1) ;; statistical mode, BM25 ranking only (faster but worse quality)
(defconstant +sph-rank-none+ 2) ;; no ranking, all matches get a weight of 1
(defconstant +sph-rank-wordcount+ 3) ;; simple word-count weighting, rank is a weighted sum of per-field keyword occurence counts

;; known sort modes
(defconstant +sph-sort-relevance+ 0)
(defconstant +sph-sort-attr-desc+ 1)
(defconstant +sph-sort-attr-asc+ 2)
(defconstant +sph-sort-time-segments+ 3)
(defconstant +sph-sort-extended+ 4)
(defconstant +sph-sort-expr+ 5)

;; known filter types
(defconstant +sph-filter-values+ 0)
(defconstant +sph-filter-range+ 1)
(defconstant +sph-filter-floatrange+ 2)

;; known attribute types
(defconstant +sph-attr-none+ 0)
(defconstant +sph-attr-integer+ 1)
(defconstant +sph-attr-timestamp+ 2)
(defconstant +sph-attr-ordinal+ 3)
(defconstant +sph-attr-bool+ 4)
(defconstant +sph-attr-float+ 5)
(defconstant +sph-attr-bigint+ 6)

;; SPH_ATTR_MULTI           = 0X40000000L

(defconstant +sph-attr-multi+ #x40000000)


;; SPH_ATTR_TYPES = (SPH_ATTR_NONE,
;;                SPH_ATTR_INTEGER,
;;                SPH_ATTR_TIMESTAMP,
;;                SPH_ATTR_ORDINAL,
;;                SPH_ATTR_BOOL,
;;                SPH_ATTR_FLOAT,
;;                SPH_ATTR_BIGINT,
;;                SPH_ATTR_MULTI)

(defconstant +sph-attr-types+ (list +sph-attr-none+
                  +sph-attr-integer+
                  +sph-attr-timestamp+
                  +sph-attr-ordinal+
                  +sph-attr-bool+
                  +sph-attr-float+
                  +sph-attr-bigint+
                  +sph-attr-multi+))

;; known grouping functions
(defconstant +sph-groupby-day+ 0)
(defconstant +sph-groupby-week+ 1)
(defconstant +sph-groupby-month+ 2)
(defconstant +sph-groupby-year+ 3)
(defconstant +sph-groupby-attr+ 4)
(defconstant +sph-groupby-attrpair+ 5)

