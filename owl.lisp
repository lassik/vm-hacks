;;;; Reimplementation of Owl Lisp in Common Lisp

(defmacro while (test &body body)
  `(loop while ,test do (progn ,@body)))

(defun << (a b)
  (ash a b))

(defun >> (a b)
  (ash a (- b)))

;;

(defconstant W 2) ;;sizeof(word)

(defvar *heap* nil)
(defvar *hp* 0)

(defun hp ()
  (elt *heap* *hp*))

(defun hp++ ()
  (prog1 (elt *heap* *hp*)
         (incf *hp*)))

(defun load-heap (filename)
  (with-open-file (file filename :element-type '(unsigned-byte 8))
    (let* ((size (file-length file))
           (heap (make-array size :element-type '(unsigned-byte 8))))
      (assert (= size (read-sequence heap file)))
      (setf *heap* heap *hp* 0)
      t)))

;;; FASL decoding

(defun get-nat ()
  (let ((result 0))
    (loop (progn (let ((i (hp++))
                       (new (<< result 7)))
                   (assert (= result (>> new 7)))   ; overflow check
                   (setf result (+ new (logand i 127)))
                   (when (zerop (logand i 128)) (return result)))))))

(defun get-obj-metrics ()
  (let ((nwords 0) (nobjs 0))
    (while (not (zerop (hp)))
      (ecase (hp++)
        (1
         (hp++)
         (let ((size (get-nat)))
           (incf nobjs)
           (incf nwords size)
           (dotimes (_ size)
             (when (zerop (hp))
               (incf *hp* 2))
             (get-nat))))
        (2
         (hp++)
         (let* ((bytes (get-nat))
                (size (if (zerop (mod bytes W))
                          (+ 1 (truncate bytes W))
                          (+ 2 (truncate bytes W)))))
           (incf *hp* bytes)
           (incf nobjs)
           (incf nwords size)))))
    (values nwords nobjs)))
