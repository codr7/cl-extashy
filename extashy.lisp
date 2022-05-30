(defpackage extashy
  (:use cl)
  (:export *default-slot-count*
	   ^= ^length list=
	   bench find-key hash key-count new-table remove-key slot-count slot-index))

(in-package extashy)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(define-symbol-macro *default-slot-count* 32)

(defmethod ^= (x y)
  (eq x y))

(defun list= (x y)
  (if (and x y)
      (and (^= (first x) (first y)) (list= (rest x) (rest y)))
      (not (or x y))))

(defmethod ^= ((x list) (y list))
  (list= x y))

(defmethod ^= ((x number) (y number))
  (= x y))

(defmethod ^= ((x string) (y string))
  (string= x y))

(defmethod ^length (obj)
  (length obj))

(defmethod ^length ((tbl hash-table))
  (hash-table-count tbl))

(defstruct table
  (eq (error "Missing eq") :type function)
  (slots (error "Missing slots") :type vector)
  (key-count 0 :type fixnum))

(defmethod new-table (&key (eq #'^=) (slot-count *default-slot-count*))
  (make-table :eq eq :slots (make-array slot-count :initial-element nil)))

(defun slot-count (tbl)
  (length (table-slots tbl)))

;;TODO
;;Add resize/setf slot-count

(defun key-count (tbl)
  (table-key-count tbl))

(defmethod ^length ((tbl table))
  (key-count tbl))

(defmethod hash (key)
  (sxhash key))

(defun slot-index (tbl key)
  (mod (hash key) (length (table-slots tbl))))

(defun find-key (tbl key &key (default nil))
  (let* ((i (slot-index tbl key))
	 (vs (aref (table-slots tbl) i)))
    (when vs
      (dotimes (i (length vs))
	(let ((v (aref vs i)))
	  (when (funcall (table-eq tbl) (first v) key)
	    (return-from find-key (rest v)))))))
  default)

(defun (setf find-key) (val tbl key)
  (let* ((i (slot-index tbl key))
	 (ts (table-slots tbl))
	 (vs (aref ts i)))
    (if vs
	(dotimes (i (length vs))
	  (let ((v (aref vs i)))
	    (when (funcall (table-eq tbl) (first v) key)
	      (rplacd (aref vs i) val)
	      (return-from find-key))))
	(progn
	  (setf vs (make-array 1 :adjustable t :fill-pointer 0))
	  (setf (aref ts i) vs)))
    (vector-push-extend (cons key val) vs)
    (incf (table-key-count tbl))))

(defun remove-key (tbl key &key (default nil))
  (let* ((i (slot-index tbl key))
	 (vs (aref (table-slots tbl) i)))
    (when vs
      (dotimes (i (length vs))
	(let ((v (aref vs i)))
	  (when (funcall (table-eq tbl) (first v) key)
	    (when (< i (1- (length vs)))
	      (replace vs (subseq vs (1+ i)) :start1 i))
	    (vector-pop vs)
	    (decf (table-key-count tbl))
	    (return-from remove-key (rest v)))))))
  default)

(defun bench (reps)
  (time
   (let ((tbl (new-table :slot-count (* reps 2))))
     (dotimes (i reps)
       (setf (find-key tbl i) i))

     (assert (= (key-count tbl) reps))

     (dotimes (i reps)
       (assert (= (find-key tbl i) i)))

     (dotimes (i reps)
       (remove-key tbl i))

     (dotimes (i reps)
       (assert (null (find-key tbl i))))

     (assert (zerop (key-count tbl)))))

  (time
   (let ((tbl (make-hash-table)))
     (dotimes (i reps)
       (setf (gethash i tbl) i))

     (assert (= (hash-table-count tbl) reps))

     (dotimes (i reps)
       (assert (= (gethash i tbl) i)))

     (dotimes (i reps)
       (remhash i tbl))

     (dotimes (i reps)
       (assert (null (gethash i tbl))))

     (assert (zerop (hash-table-count tbl))))))


