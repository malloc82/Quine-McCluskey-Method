
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

(defun quine-mccluskey (sample-str sum-list)
  (let* ((sample-chars (string-to-chars sample-str))
         (table (make-hash-table :test #'equal))
         (bin (make-hash-table :test #'equal))
         (str-format (format nil "~c~d,'0b" #\~ (length sample-str)))
         (lst nil)
         (final nil)
         (xor-fn (chars-gate :inputs (a b)
                             :table ((0 0 0)
                                     (0 1 1)
                                     (1 0 1)
                                     (1 1 0)
                                     (- - -))))
         (mask-fn (chars-gate :inputs (a b)
                              :table ((0 0 0)
                                      (0 1 -)
                                      (1 0 -)
                                      (1 1 1)
                                      (- - -)))))
    (labels ((order (lst)
               (if (or (null lst) (null (cdr lst)))
                   lst
                   (let ((pivot (first (first lst))))
                     (append (order (remove-if-not #'(lambda (x) (< (first x) pivot)) lst))
                             (remove-if-not #'(lambda (x) (= (first x) pivot)) lst)
                             (order (remove-if-not #'(lambda (x) (> (first x) pivot)) lst))))))
             (qm-rec (groups)
               (when groups
                 (let ((next-round nil))
                   (mapl #'(lambda (sublst)
                             (let ((new-group nil)
                                   (current (second (first sublst)))
                                   (next    (second (second sublst))))
                               (when (and (cdr sublst)
                                          (= (1+ (car (first sublst)))
                                             (car (second sublst))))
                                 (mapc #'(lambda (x)
                                           (mapc #'(lambda (y)
                                                     (let ((result (funcall xor-fn x y)))
                                                       (when (and result
                                                                  (= (count-elem result #\1) 1))
                                                         (let ((new-entry (funcall mask-fn x y)))
                                                           (when (null (gethash new-entry table))
                                                             (setf (gethash new-entry table) 0)
                                                             (push new-entry new-group))
                                                           (setf (gethash x table) 2)
                                                           (setf (gethash y table) 1)))))
                                                 next))
                                           current)
                                 (when new-group
                                   (push (cons (car (first sublst)) (list new-group)) next-round)))
                               (mapc #'(lambda (x)
                                         (let ((hash-check (gethash x table)))
                                           (remhash x table)
                                           (when (= hash-check 0)
                                             (push x final))))
                                     current)))
                         groups)
                   (qm-rec (nreverse next-round))))))
      (mapc #'(lambda (x)
                (let ((str (format nil str-format x))
                      (chars nil))
                  (loop for c across str
                     if (equal c #\1) count c into count do (push c chars)
                     finally (let ((val (nreverse chars)))
                               (setf (gethash val table) 0)
                               (push val (gethash count bin))))))
            sum-list)
      (maphash #'(lambda (key val)
                   (push (cons key (list val)) lst))
               bin)
      (qm-rec (order lst)))
    
    (values
     (mapcar #'(lambda (entry)
                 (let ((result nil))
                   (mapcar #'(lambda (char bin)
                               (cond ((equal bin #\1)  (push char result))
                                     ((equal bin #\0)  (push char result) (push #\' result))))
                           sample-chars entry)
                   (coerce (nreverse result) 'string)))
             final)
     (mapcar #'(lambda (entry) (coerce entry 'string)) final))))

