;; Author :    Ritchie Cai
;; Date :      Oct 26, 2011
;;
;; Description:
;;     This program minimize boolean terms using Quine-McCluskey method.
;;     Main function : quine-mccluskey


(defun print-table (table)
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (push (list (coerce key 'string)
                             value)
                       result))
             table)
    (format t "~%")
    (format t "~{~a~%~}" (reverse result))
    (format t "-------------------~%")))

(defun print-table2 (table)
  (maphash #'(lambda (key value)
               (format t "~a : ~a ~%" key value))
           table))

(defmacro string-to-chars (str &optional pos-lst)
  (if (null pos-lst)
      `(mapcar #'(lambda (index) (schar ,str index))
               (loop for i from 0 upto (1- (length ,str)) collect i))
      `(mapcar #'(lambda (index) (schar ,str index))
               ,pos-lst)))

(defmacro chars-gate (&key (inputs nil) (table nil))
  (let ((cond-body `((t (setq valid nil))))
        (input-len (length inputs)))
    (loop
         for entry in table do
         (let ((varlist inputs)
               (entry-body nil))
           (loop
                for i from 1 to input-len
                do (push `(equal ,(pop varlist) ,(schar (write-to-string (pop entry)) 0)) entry-body))
           (push (append (list (append (list 'and) (reverse entry-body)))
                         (list `,(schar (write-to-string (pop entry)) 0)))
                 cond-body))
         finally (push 'cond cond-body))
    `(lambda ,inputs
       (let* ((valid  t)
              (result (map 'list #'(lambda ,inputs ,cond-body) ,@inputs)))
         (if valid result nil)))))

(defmacro count-elem (lst e)
  `(loop
      for elem in ,lst
      counting (equal ,e elem) into total
      finally (return total)))

(defun order (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((pivot (first (first lst))))
        (append (order (remove-if-not #'(lambda (x) (< (first x) pivot)) lst))
                (remove-if-not #'(lambda (x) (= (first x) pivot)) lst)
                (order (remove-if-not #'(lambda (x) (> (first x) pivot)) lst))))))

(defun order2 (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((pivot (first lst)))
        (append (order2 (remove-if-not #'(lambda (x) (< x pivot)) lst))
                (remove-if-not #'(lambda (x) (= x pivot)) lst)
                (order2 (remove-if-not #'(lambda (x) (> x pivot)) lst))))))


(defvar *xor-fn* (chars-gate :inputs (a b)
                             :table ((0 0 0)
                                     (0 1 1)
                                     (1 0 1)
                                     (1 1 0)
                                     (- - -))))

(defvar *mask-fn* (chars-gate :inputs (a b)
                              :table ((0 0 0)
                                      (0 1 -)
                                      (1 0 -)
                                      (1 1 1)
                                      (- - -))))

(defun init-table (sum-list str-format &key (print-fn nil))
  (let ((table (make-hash-table :test #'equal))
        (count-list nil))
    (mapc #'(lambda (x)
              (let ((str (format nil str-format x))
                    (chars nil))
                (loop for c across str
                   if (equal c #\1) count c into count
                   do (push c chars)
                   finally (let ((val (nreverse chars)))
                             (push count count-list)
                             (push val (getf (gethash count table) :current))
                             ;; (setf (get (gethash count table) :current)
                             ;;       (push val (get (gethash count table) :current)))
                             ))))
          sum-list)
    (setf (gethash :sorted-keys table) (remove-duplicates (order2 count-list)))
    (when print-fn
      (funcall print-fn table))
    table))

(defun mark (k1-list k2-list)
  (let ((next-round nil)
        (used-k1 nil)
        (used-k2 nil))
    (loop :for x in k1-list :do
       (loop :for y in k2-list :do
          (let ((diff (funcall *xor-fn* x y)))
            (when (and diff
                       (= (count-elem diff #\1) 1))
              (push x used-k1)
              (push y used-k2)
              (push (funcall *mask-fn* x y) next-round)))))
    (values
     (remove-duplicates next-round)
     (remove-duplicates used-k1)
     (remove-duplicates used-k2))))

(defun qm2 (table &key (print-fn nil))
  (let ((keys (gethash :sorted-keys table)))
    (loop
       :for (k1 k2) :on keys :by #'cdr :until (null k2)
       :with k1-cache = (getf (gethash (first keys) table) :current)
       :do (when (= (- k2 k1) 1)
             (multiple-value-bind (k1-next k1-used k2-used)
                 (mark k1-cache (getf (gethash k2 table) :current))
               ;; k1
               (setf (getf (gethash k1 table) :unused)
                     (set-difference (set-difference k1-cache k1-used)
                                     (getf (gethash k1 table) :used)))
               (setf (getf (gethash k1 table) :current) k1-next)

               ;; k2
               (setf (getf (gethash k2 table) :used) k2-used)
               (setf (getf (gethash k2 table) :unused)
                     (set-difference (getf (gethash k2 table) :current)
                                     k2-used))
               (setf k1-cache (getf (gethash k2 table) :current))
               (setf (getf (gethash k2 table) :current) nil))))
    (loop :for k in keys
       :do (when (getf (gethash k table) :unused)
             (setf (getf (gethash k table) :saved)
                   (nconc (getf (gethash k table) :unused)
                          (getf (gethash k table) :saved)))
             (setf (getf (gethash k table) :unused) nil))
       :if (getf (gethash k table) :current) :collect k into newkeys
       :finally (setf (gethash :sorted-keys table) (order2 newkeys)))
    (when print-fn
      (funcall print-fn table))))

(defun mask-chars (str mask)
  (let ((result nil))
    (loop
       :for c :in (coerce str 'list)
       :for m :in mask
       :do (cond ((equal m #\1) (push #\' result) (push c result))
                 ((equal m #\0) (push c result))))
    (coerce (reverse result) 'string)))

(defun quine-mccluskey2 (sample-str sum-list &key (print-fn nil))
  (let* ((str-format (format nil "~c~d,'0b" #\~ (length sample-str)))
         (table (init-table sum-list str-format :print-fn print-fn)))
    (loop
       :while (gethash :sorted-keys table)
       :do (qm2 table :print-fn print-fn)
       :finally (let ((result nil))
                  (print-table2 table)
                  (loop
                     :for val being the hash-values of table
                     :if (getf val :saved) :do (setf result
                                                     (nconc (getf val :saved) result)))
                  (setq result (remove-duplicates result :test #'equal))
                  (return
                    (values
                     (loop :for mask :in result :collect (mask-chars sample-str mask))
                     (loop :for mask :in result :collect (coerce mask 'string))))))))

