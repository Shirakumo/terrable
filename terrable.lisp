#|
 This file is a part of Terrable
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.terrable)

(define-condition terrable-condition (condition)
  ())

(define-condition invalid-header (terrable-condition error)
  ((header :initarg :header :reader header))
  (:report (lambda (c s) (format s "Unexpected header string: ~a" (header c)))))

(define-condition unknown-chunk-type (terrable-condition warning)
  ((chunk-type :initarg :chunk-type :reader chunk-type))
  (:report (lambda (c s) (format s "Unknown chunk type: ~a" (chunk-type c)))))

(define-condition unknown-curvature-type (terrable-condition warning)
  ((curvature-type :initarg :curvature-type :reader curvature-type))
  (:report (lambda (c s) (format s "Unknown curvature type: ~a" (curvature-type c)))))

(defclass terrain ()
  ((width :initform NIL :accessor width)
   (height :initform NIL :accessor height)
   (scale :initform (list 30 30 30) :accessor scale)
   (curve-radius :initform 6370 :accessor curve-radius)
   (curve-mode :initform :flat :accessor curve-mode)
   (height-base :initform 0 :accessor height-base)
   (height-scale :initform 1f0 :accessor height-scale)
   (data :initform NIL :accessor data)))

(defun read-marker (stream buffer &key start end)
  (loop for i from (or start 0) below (or end (length buffer))
        for code = (fast-io:readu8 stream)
        do (setf (aref buffer i) (code-char code))
        finally (return buffer)))

(defun check-header (stream buffer expected)
  (read-marker stream buffer)
  (unless (string= buffer expected)
    (error 'invalid-header :header buffer)))

(defun read-chunk (stream buffer terrain)
  (let ((marker (read-marker stream buffer :end 4)))
    (flet ((markerp (thing)
             (string= marker thing :end2 4)))
      (cond ((markerp "XPTS")
             (setf (width terrain) (fast-io:readu16-le stream)))
            ((markerp "YPTS")
             (setf (height terrain) (fast-io:readu16-le stream)))
            ((markerp "SIZE")
             (let ((n (1+ (fast-io:readu16-le stream))))
               (unless (height terrain) (setf (height terrain) n))
               (unless (width terrain) (setf (width terrain) n))
               n))
            ((markerp "SCAL")
             (let ((x (fast-io:readu32-le stream))
                   (y (fast-io:readu32-le stream))
                   (z (fast-io:readu32-le stream)))
               (setf (scale terrain) (list (ieee-floats:decode-float32 x)
                                           (ieee-floats:decode-float32 y)
                                           (ieee-floats:decode-float32 z)))))
            ((markerp "CRAD")
             (let ((c (fast-io:readu32-le stream)))
               (setf (curve-radius terrain) (ieee-floats:decode-float32 c))))
            ((markerp "CRVM")
             (let ((m (fast-io:readu32-le stream)))
               (setf (curve-mode terrain) (case m
                                            (0 :flat)
                                            (1 :draped)
                                            (T (warn 'unknown-curvature-type :curvature-type m)
                                             m)))))
            ((markerp "ALTW")
             (setf (height-scale terrain) (float (/ (fast-io:read16-le stream) 65536) 0f0))
             (setf (height-base terrain) (fast-io:read16-le stream))
             (let ((data (static-vectors:make-static-vector (* (width terrain) (height terrain) 2))))
               (fast-io:fast-read-sequence data stream)
               (setf (data terrain) data)))
            ((markerp "EOF ")
             NIL)
            (T
             (warn 'unknown-chunk-type :chunk-type (subseq marker 0 4))
             terrain)))))

(defun read-buffer (stream)
  (let ((buffer (make-array 8 :element-type 'base-char))
        (terrain (make-instance 'terrain)))
    (check-header stream buffer "TERRAGEN")
    (check-header stream buffer "TERRAIN ")
    (loop while (read-chunk stream buffer terrain))
    terrain))

(defgeneric read-terragen (input))

(defmethod read-terragen ((buffer fast-io:input-buffer))
  (read-buffer buffer))

(defmethod read-terragen ((stream stream))
  (fast-io:with-fast-input (buffer NIL stream)
    (read-buffer buffer)))

(defmethod read-terragen ((pathname pathname))
  (with-open-file (stream pathname :direction :input
                                   :element-type '(unsigned-byte 8))
    (read-terragen pathname)))

(defmethod read-terragen ((string string))
  (read-terragen (pathname string)))

(defmethod read-terragen ((vector vector))
  (fast-io:with-fast-input (buffer vector)
    (read-buffer buffer)))
