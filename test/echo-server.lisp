;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Multiplexer example, adapted from Juho Snellman's version for SBCL
;;;     which is available at http://jsnell.iki.fi/tmp/echo-server.lisp.
;;;
;;;; See the LICENSE file for licensing information.

(in-package #:cl-sphinx-search-test)


(defparameter *port* 9999)
(defvar *event-base* nil)
(defvar *sockets* (make-hash-table))
(defvar *counter* 0)

(defun add-socket (socket)
  (push socket *sockets*))

(defun remove-socket (socket)
  (removef *sockets* socket))

(defun close-socket (socket)
  (let ((fd (iolib.sockets:socket-os-fd socket)))
    (ignore-errors (iomux:remove-fd-handlers *event-base* fd))
    (remove-socket socket)
    (close socket)))

(defun make-echoer (stream id disconnector)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (handler-case
        (let ((line (read-line stream)))
          (cond ((string= line "quit")
                 (funcall disconnector))
                (t
                 (format t "~A: ~A~%" id line)

          (sockets:send-to stream #(1 2 3 4 5 6 7 8))
          (finish-output stream)
                 (format stream "~A: ~A~%" id line)
                 (with-open-file (stream "/tmp/echo-server.log" :direction :output :if-exists :append
                                         :element-type '(unsigned-byte 8))
                   (write-sequence (string-to-octets line) stream))
                 (ignore-some-conditions (iolib.streams:hangup)
                   (finish-output stream)))))
      (end-of-file ()
        (funcall disconnector)))))

(defun make-disconnector (socket id)
  (lambda ()
    (format t "~A: closing~%" id)
    (close-socket socket)))

(defun serve (socket id)
  (iomux:set-io-handler *event-base*
                        (iolib.sockets:socket-os-fd socket)
                        :read
                        (make-echoer socket id
                                     (make-disconnector socket id))))

(defun make-listener-handler (socket)
  (lambda (fd event exception)
    (declare (ignore fd exception))
    (block nil
      (when (eql :timeout event)
        (warn "Got a server timeout!")
        (return))
      (let ((client (iolib.sockets:accept-connection socket)))
        (when client
          (setf (iolib.streams:fd-non-blocking client) t)
          (add-socket client)
          (sockets:send-to client #(80 70 60 50))
          (finish-output client)
          (incf *counter*)
          (format t "Accepted client ~A~%" *counter*)
          (serve client *counter*))))))

(defun start-echo-server (host port)
  (let ((socket
         (iolib.sockets:make-socket :connect :passive :address-family :internet :type :stream
                                    :local-host host :local-port port
                                    :backlog 5 :reuse-address t
                                    :external-format '(:latin-1 :eol-style :crlf) :ipv6 nil)))
    (setf *counter* 0
          *sockets* nil)
    (unwind-protect-case ()
        (progn
          (setf (iolib.streams:fd-non-blocking socket) t)
          (add-socket socket)
          (iomux:set-io-handler *event-base*
                                (iolib.sockets:socket-os-fd socket)
                                :read
                                (make-listener-handler socket)
                                :timeout 15))
      (:abort (close socket)))
    socket))

(defun close-all-sockets ()
  (map 'nil #'close-socket *sockets*))

(defun run-server (&key (host iolib.sockets:+ipv4-unspecified+)
                   (port *port*) (new-process t) (timeout 10))
  (flet ((%run-server ()
           (unwind-protect
                (progn
                  (setf *event-base* (make-instance 'iomux:event-base))
                  (with-open-stream (sock (start-echo-server host port))
                    (iomux:event-dispatch *event-base* :timeout timeout)))
             (close-all-sockets)
             (close *event-base*))))
    (let ((iolib.sockets:*ipv6* nil))
      (if new-process
          (bt:make-thread #'%run-server)
          (%run-server)))))
