

(defun bubble-pass-functional (lst)
  "Performs one pass of bubble sort.
   Bubbles the largest element to the end of the list."
  (cond
    ((or (null lst) (null (cdr lst))) lst)
     ;; If current > next, swap them (construct new list order)
    ((> (car lst) (cadr lst))
     (cons (cadr lst)
           (bubble-pass-functional (cons (car lst) (cddr lst)))))
    ;; Else, keep order and continue
    (t 
     (cons (car lst)
           (bubble-pass-functional (cdr lst))))))

(defun bubble-sort-iter (lst n)
  "Helper function to perform N passes."
  (if (zerop n)
      lst
      (bubble-sort-iter (bubble-pass-functional lst) (1- n))))

(defun bubble-sort-functional (lst)
  "Entry point for functional bubble sort.
   Variant 2 requires 'without optimizations', so we run N passes
   regardless of whether the list is already sorted."
  (bubble-sort-iter lst (length lst)))


;;; ===================================================================
;;; IMPERATIVE IMPLEMENTATION
;;; Requirement: Loops, destructive ops, copy-list [cite: 365, 367]
;;; ===================================================================

(defun bubble-sort-imperative (lst)
  "Bubble sort - imperative implementation using loops.
   Destructive modifications on a copy of the list."
  (let ((arr (copy-list lst)) ;; Copy strictly required 
        (n (length lst)))
    (dotimes (i (1- n))
      (dotimes (j (- n i 1))
        (let ((curr (nth j arr))
              (next (nth (1+ j) arr)))
          (when (> curr next)
            ;; Destructive swap using rotatef 
            (rotatef (nth j arr) (nth (1+ j) arr))))))
    arr))


(defun check-bubble-sort (name input expected sort-fn)
  "Test function to compare actual result with expected."
  (let ((result (funcall sort-fn input)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name)
    (unless (equal result expected)
      (format t "  Input: ~a~%  Expected: ~a~%  Got: ~a~%" 
              input expected result))))

(defun test-bubble-sort-functional ()
  (format t "~%=== Testing Functional Bubble Sort (Variant 2) ===~%")
  (check-bubble-sort "test 1: basic random" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9) #'bubble-sort-functional)
  (check-bubble-sort "test 2: empty" nil nil #'bubble-sort-functional)
  (check-bubble-sort "test 3: single atom" '(42) '(42) #'bubble-sort-functional)
  (check-bubble-sort "test 4: sorted" '(1 2 3) '(1 2 3) #'bubble-sort-functional)
  (check-bubble-sort "test 5: reversed" '(3 2 1) '(1 2 3) #'bubble-sort-functional))

(defun test-bubble-sort-imperative ()
  (format t "~%=== Testing Imperative Bubble Sort (Variant 2) ===~%")
  (check-bubble-sort "test 1: basic random" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9) #'bubble-sort-imperative)
  (check-bubble-sort "test 2: empty" nil nil #'bubble-sort-imperative)
  (check-bubble-sort "test 3: single atom" '(42) '(42) #'bubble-sort-imperative)
  (check-bubble-sort "test 4: sorted" '(1 2 3) '(1 2 3) #'bubble-sort-imperative)
  (check-bubble-sort "test 5: reversed" '(3 2 1) '(1 2 3) #'bubble-sort-imperative))

(defun test-all ()
  (test-bubble-sort-functional)
  (test-bubble-sort-imperative))