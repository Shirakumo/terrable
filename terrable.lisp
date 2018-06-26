#|
 This file is a part of Terrable
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.terrable)

(define-condition terrable-condition (condition)
  ((position :initarg :position :reader stream-position))
  (:report (lambda (c s) (format s "Terragen file problem at ~d."
                                 (stream-position c)))))

(define-condition invalid-header (terrable-condition error)
  ((header :initarg :header :reader header))
  (:report (lambda (c s) (format s "Unexpected header string at ~d: ~a"
                                 (stream-position c) (header c)))))

(define-condition unknown-chunk-type (terrable-condition warning)
  ((chunk-type :initarg :chunk-type :reader chunk-type))
  (:report (lambda (c s) (format s "Unknown chunk type at ~d: ~a"
                                 (stream-position c) (chunk-type c)))))

(define-condition unknown-curvature-type (terrable-condition warning)
  ((curvature-type :initarg :curvature-type :reader curvature-type))
  (:report (lambda (c s) (format s "Unknown curvature type at ~d: ~a"
                                 (stream-position c) (curvature-type c)))))

(defclass terrain ()
  ((width :initform NIL :accessor width)
   (height :initform NIL :accessor height)
   (scale :initform (list 30 30 30) :accessor scale)
   (curve-radius :initform 6370f0 :accessor curve-radius)
   (curve-mode :initform :flat :accessor curve-mode)
   (height-base :initform 0 :accessor height-base)
   (height-scale :initform 1f0 :accessor height-scale)
   (data :initform NIL :accessor data)))

(defmethod print-object ((terrain terrain) stream)
  (print-unreadable-object (terrain stream :type T :identity T)
    (format stream "~dx~d ~a" (width terrain) (height terrain) (curve-mode terrain))))

(defun read-marker (buffer marker &key start end)
  (declare (type (simple-array base-char (8)) marker))
  (declare (optimize speed))
  (loop for i from (or start 0) below (or end (length marker))
        for code = (fast-io:readu8 buffer)
        do (setf (aref marker i) (code-char code))
        finally (return marker)))

(defun check-header (buffer marker expected)
  (read-marker buffer marker)
  (unless (string= marker expected)
    (error 'invalid-header
           :position (fast-io:buffer-position buffer)
           :header marker)))

(defun read-chunk (buffer marker terrain)
  (declare (type fast-io:input-buffer buffer)
           (type (simple-array base-char (8)) marker)
           (type terrain terrain))
  (declare (optimize speed))
  (handler-case (read-marker buffer marker :end 4)
    (end-of-file (e)
      (declare (ignore e))
      (return-from read-chunk NIL)))
  (flet ((markerp (thing)
           (string= marker thing :end1 4)))
    (cond ((markerp "XPTS")
           (setf (width terrain) (fast-io:readu16-le buffer)))
          ((markerp "YPTS")
           (setf (height terrain) (fast-io:readu16-le buffer)))
          ((markerp "SIZE")
           (let ((n (1+ (fast-io:readu16-le buffer))))
             (fast-io:fast-read-byte buffer)
             (fast-io:fast-read-byte buffer)
             (unless (height terrain) (setf (height terrain) n))
             (unless (width terrain) (setf (width terrain) n))
             n))
          ((markerp "SCAL")
           (let ((x (fast-io:readu32-le buffer))
                 (y (fast-io:readu32-le buffer))
                 (z (fast-io:readu32-le buffer)))
             (setf (scale terrain) (list (ieee-floats:decode-float32 x)
                                         (ieee-floats:decode-float32 y)
                                         (ieee-floats:decode-float32 z)))))
          ((markerp "CRAD")
           (let ((c (fast-io:readu32-le buffer)))
             (setf (curve-radius terrain) (ieee-floats:decode-float32 c))))
          ((markerp "CRVM")
           (let ((m (fast-io:readu32-le buffer)))
             (setf (curve-mode terrain) (case m
                                          (0 :flat)
                                          (1 :draped)
                                          (T (warn 'unknown-curvature-type
                                                   :position (fast-io:buffer-position buffer)
                                                   :curvature-type m)
                                           m)))))
          ((markerp "ALTW")
           (setf (height-scale terrain) (/ (float (fast-io:read16-le buffer) 0f0) 65536f0))
           (setf (height-base terrain) (fast-io:read16-le buffer))
           (let* ((size (* (the (unsigned-byte 16) (width terrain))
                           (the (unsigned-byte 16) (height terrain))))
                  (data (static-vectors:make-static-vector size :element-type '(signed-byte 16))))
             (setf (data terrain) data)
             (tg:finalize terrain (lambda () (static-vectors:free-static-vector data)))
             (loop for i of-type (unsigned-byte 32) from 0 below size
                   do (setf (aref data i) (fast-io:read16-le buffer)))
             terrain))
          ((markerp "EOF ")
           NIL)
          (T
           (warn 'unknown-chunk-type
                 :position (fast-io:buffer-position buffer)
                 :chunk-type (subseq marker 0 4))
           terrain))))

(defun read-buffer (buffer)
  (let ((marker (make-array 8 :element-type 'base-char))
        (terrain (make-instance 'terrain)))
    (check-header buffer marker "TERRAGEN")
    (check-header buffer marker "TERRAIN ")
    (loop while (read-chunk buffer marker terrain))
    terrain))

(defgeneric read-terrain (input))

(defmethod read-terrain ((buffer fast-io:input-buffer))
  (read-buffer buffer))

(defmethod read-terrain ((stream stream))
  (fast-io:with-fast-input (buffer NIL stream)
    (read-buffer buffer)))

(defmethod read-terrain ((pathname pathname))
  (with-open-file (stream pathname :direction :input
                                   :element-type '(unsigned-byte 8))
    (read-terrain stream)))

(defmethod read-terrain ((string string))
  (read-terrain (pathname string)))

(defmethod read-terrain ((vector vector))
  (fast-io:with-fast-input (buffer vector)
    (read-buffer buffer)))

(defmethod free-terrain ((terrain terrain))
  (tg:cancel-finalization terrain)
  (static-vectors:free-static-vector (data terrain))
  (setf (data terrain) NIL))

(defun write-marker (buffer marker)
  (loop for char across marker
        do (fast-io:writeu8 (char-code char) buffer)))

(defun write-buffer (buffer terrain)
  (write-marker buffer "TERRAGEN")
  (write-marker buffer "TERRAIN ")
  (write-marker buffer "SIZE")
  (fast-io:writeu16-le (1- (min (height terrain) (width terrain))) buffer)
  (fast-io:writeu16-le 0 buffer)
  (write-marker buffer "XPTS")
  (fast-io:writeu16-le (width terrain) buffer)
  (write-marker buffer "YPTS")
  (fast-io:writeu16-le (height terrain) buffer)
  (write-marker buffer "SCAL")
  (destructuring-bind (x y z) (scale terrain)
    (fast-io:writeu32-le (ieee-floats:encode-float32 x) buffer)
    (fast-io:writeu32-le (ieee-floats:encode-float32 y) buffer)
    (fast-io:writeu32-le (ieee-floats:encode-float32 z) buffer))
  (write-marker buffer "CRAD")
  (fast-io:writeu32-le (ieee-floats:encode-float32 (curve-radius terrain)) buffer)
  (write-marker buffer "CRVM")
  (fast-io:writeu32-le (case (curve-mode terrain)
                         (:flat 0)
                         (:draped 1)
                         (T (curve-mode terrain)))
                       buffer)
  (write-marker buffer "ALTW")
  (fast-io:write16-le (round (* (height-scale terrain) 65536)) buffer)
  (fast-io:write16-le (height-base terrain) buffer)
  (loop for el across (data terrain)
        do (fast-io:write16-le el buffer))
  (write-marker buffer "EOF ")
  buffer)

(defgeneric write-terrain (terrain output))

(defmethod write-terrain ((terrain terrain) (buffer fast-io:output-buffer))
  (write-buffer buffer terrain))

(defmethod write-terrain (terrain (stream stream))
  (fast-io:with-fast-output (buffer stream)
    (write-terrain terrain buffer)))

(defmethod write-terrain (terrain (pathname pathname))
  (with-open-file (stream pathname :direction :output
                                   :element-type '(unsigned-byte 8))
    (write-terrain terrain stream)
    pathname))

(defmethod write-terrain (terrain (output symbol))
  (fast-io:with-fast-output (buffer output)
    (write-terrain terrain buffer)))
