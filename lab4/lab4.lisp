;;; Lab 4: Higher-Order Functions and Closures
;;; Part 1: Bubble Sort (Variant 2) with KEY and TEST parameters
;;; Part 2: add-prev-reducer (Variant 2) closure

;;; ===================================================================
;;; PART 1: BUBBLE SORT WITH KEY AND TEST PARAMETERS
;;; ===================================================================

;;; ---------------- FUNCTIONAL IMPLEMENTATION ----------------

(defun bubble-sort-functional (lst &key (test #'<) key)
  "Bubble sort - functional implementation with KEY and TEST parameters.
   
   KEY: function to extract comparison value from each element
        (executed exactly once per element - minimal).
   TEST: comparison function (default: #'< for ascending order).
   
   Higher-order functions used: MAPCAR for applying KEY."
  (if key
      ;; With KEY: create (element . key-value) pairs once, sort, extract elements
      (let ((keyed-pairs (mapcar (lambda (elt) (cons elt (funcall key elt))) lst)))
        (mapcar #'car (bubble-sort-with-test keyed-pairs 
                                             (lambda (p1 p2) 
                                               (funcall test (cdr p1) (cdr p2))))))
      ;; Without KEY: sort elements directly
      (bubble-sort-with-test lst test)))

(defun bubble-sort-with-test (lst test)
  "Helper function: bubble sort using TEST for comparison.
   Implements classic bubble sort without optimizations (Variant 2)."
  (labels ((bubble-pass (lst)
             "One pass: swap adjacent elements if out of order."
             (cond
               ((or (null lst) (null (cdr lst))) lst)
               ;; If second < first (using test), swap them
               ((funcall test (cadr lst) (car lst))
                (cons (cadr lst)
                      (bubble-pass (cons (car lst) (cddr lst)))))
               ;; Otherwise keep order
               (t
                (cons (car lst) (bubble-pass (cdr lst))))))
           (bubble-iter (lst n)
             "Perform N passes (no early termination - Variant 2 requirement)."
             (if (zerop n)
                 lst
                 (bubble-iter (bubble-pass lst) (1- n)))))
    (bubble-iter lst (length lst))))


;;; ---------------- IMPERATIVE IMPLEMENTATION ----------------

(defun bubble-sort-imperative (lst &key (test #'<) key)
  "Bubble sort - imperative implementation with KEY and TEST parameters.
   
   KEY: function to extract comparison value from each element
        (executed exactly once per element - minimal).
   TEST: comparison function (default: #'< for ascending order).
   
   Higher-order functions used: MAPCAR for pre-computing keys.
   Uses COPY-LIST to avoid modifying original list."
  (if (null lst)
      nil
      (let* ((n (length lst))
             ;; Pre-compute all key values once (minimal execution)
             (keys (when key (mapcar key lst)))
             (arr (copy-list lst)))
        ;; Classic bubble sort: N-1 passes
        (dotimes (i (1- n))
          (dotimes (j (- n i 1))
            (let ((curr-key (if key (nth j keys) (nth j arr)))
                  (next-key (if key (nth (1+ j) keys) (nth (1+ j) arr))))
              ;; Swap if next < current (using test)
              (when (funcall test next-key curr-key)
                ;; 1. Swap the actual elements
                (rotatef (nth j arr) (nth (1+ j) arr))
                ;; 2. CRITICAL FIX: Swap the keys too!
                ;; If we don't do this, keys get out of sync with elements.
                (when key
                  (rotatef (nth j keys) (nth (1+ j) keys)))))))
        arr)))


;;; ===================================================================
;;; PART 2: ADD-PREV-REDUCER (VARIANT 2)
;;; ===================================================================

(defun add-prev-reducer (&key transform)
  "Returns a reducer function that pairs each element with its previous element.
   
   Each element becomes: (current . previous)
   - First element has NIL as previous: (elem1 . NIL)
   - Others have actual previous: (elem2 . elem1), (elem3 . elem2), etc.
   
   TRANSFORM: optional function applied to both current and previous elements.
              Executes exactly once per element (minimal).
   
   Usage:
     (reduce (add-prev-reducer) '(1 2 3) 
             :from-end nil 
             :initial-value nil)
     => ((1 . NIL) (2 . 1) (3 . 2))
     
     (reduce (add-prev-reducer :transform #'1+) '(1 2 3)
             :from-end nil
             :initial-value nil)
     => ((2 . NIL) (3 . 2) (4 . 3))
   
   Implementation notes:
   - Must use :from-end nil (process left-to-right)
   - Must use :initial-value nil
   - Uses closure to track previous element
   - Caches transformed value to minimize TRANSFORM calls"
  (let ((transformed-prev nil))  ; Cache transformed previous
    (lambda (acc current)
      ;; Transform current element
      (let* ((transformed-current (if transform 
                                      (funcall transform current)
                                      current))
             ;; Create pair with cached previous
             (new-pair (cons transformed-current transformed-prev))
             ;; Append to result list
             (result (append acc (list new-pair))))
        ;; Cache current (transformed) for next iteration's "previous"
        (setf transformed-prev transformed-current)
        result))))


;;; ===================================================================
;;; TESTING UTILITIES
;;; ===================================================================

;;; ---------------- Part 1 Tests ----------------

(defun check-sort (name input expected sort-fn &rest params)
  "Test sorting function with given parameters."
  (let ((result (apply sort-fn input params)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name)
    (unless (equal result expected)
      (format t "  Input: ~a~%  Expected: ~a~%  Got: ~a~%" 
              input expected result))))

(defun test-bubble-sort-functional-extended ()
  "Test functional bubble sort with KEY and TEST parameters."
  (format t "~%=== Testing Functional Bubble Sort (with KEY/TEST) ===~%")
  
  ;; Basic tests
  (check-sort "basic: ascending" 
              '(3 1 4 1 5) '(1 1 3 4 5) 
              #'bubble-sort-functional)
  
  ;; With KEY parameter
  (check-sort "with key: sort by absolute value" 
              '(-5 2 -1 3) '(-1 2 3 -5)
              #'bubble-sort-functional :key #'abs)
  
  (check-sort "with key: sort strings by length" 
              '("xxx" "a" "bb") '("a" "bb" "xxx")
              #'bubble-sort-functional :key #'length)
  
  ;; With TEST parameter
  (check-sort "with test: descending order" 
              '(1 2 3 4 5) '(5 4 3 2 1)
              #'bubble-sort-functional :test #'>)
  
  ;; With both KEY and TEST
  (check-sort "key + test: sort by length descending" 
              '("a" "xxx" "bb") '("xxx" "bb" "a")
              #'bubble-sort-functional :key #'length :test #'>)
  
   ;; Edge cases
  (check-sort "empty list" 
              nil nil 
              #'bubble-sort-functional)
  
  (check-sort "single element" 
              '(42) '(42)
              #'bubble-sort-functional :key #'1+))

(defun test-bubble-sort-imperative-extended ()
  "Test imperative bubble sort with KEY and TEST parameters."
  (format t "~%=== Testing Imperative Bubble Sort (with KEY/TEST) ===~%")
  
  ;; Basic tests
  (check-sort "basic: ascending" 
              '(3 1 4 1 5) '(1 1 3 4 5) 
              #'bubble-sort-imperative)
  
  ;; With KEY parameter
  (check-sort "with key: sort by absolute value" 
              '(-5 2 -1 3) '(-1 2 3 -5)
              #'bubble-sort-imperative :key #'abs)
  
  (check-sort "with key: sort strings by length" 
              '("xxx" "a" "bb") '("a" "bb" "xxx")
              #'bubble-sort-imperative :key #'length)
  
  ;; With TEST parameter
  (check-sort "with test: descending order" 
              '(1 2 3 4 5) '(5 4 3 2 1)
              #'bubble-sort-imperative :test #'>)
  
  ;; With both KEY and TEST
  (check-sort "key + test: sort by length descending" 
              '("a" "xxx" "bb") '("xxx" "bb" "a")
              #'bubble-sort-imperative :key #'length :test #'>)
  
  ;; Edge cases
  (check-sort "empty list" 
              nil nil 
              #'bubble-sort-imperative)
  
  (check-sort "single element" 
              '(42) '(42)
              #'bubble-sort-imperative :key #'1+))


;;; ---------------- Part 2 Tests ----------------

(defun check-reducer (name input expected reducer &rest reduce-params)
  "Test reducer function with REDUCE."
  (let ((result (apply #'reduce reducer input reduce-params)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name)
    (unless (equal result expected)
      (format t "  Input: ~a~%  Expected: ~a~%  Got: ~a~%" 
              input expected result))))

(defun test-add-prev-reducer ()
  "Test add-prev-reducer closure."
  (format t "~%=== Testing add-prev-reducer (Variant 2) ===~%")
  
  ;; Basic usage
  (check-reducer "basic: (1 2 3)" 
                 '(1 2 3)
                 '((1 . nil) (2 . 1) (3 . 2))
                 (add-prev-reducer)
                 :from-end nil :initial-value nil)
  
  ;; With transform
  (check-reducer "with transform #'1+: (1 2 3)" 
                 '(1 2 3)
                 '((2 . nil) (3 . 2) (4 . 3))
                 (add-prev-reducer :transform #'1+)
                 :from-end nil :initial-value nil)
  
  ;; Edge cases
  (check-reducer "single element" 
                 '(5)
                 '((5 . nil))
                 (add-prev-reducer)
                 :from-end nil :initial-value nil)
  
  (check-reducer "empty list" 
                 nil
                 nil
                 (add-prev-reducer)
                 :from-end nil :initial-value nil)
  
  ;; More complex transform
  (check-reducer "transform with (* 2): (1 2 3)" 
                 '(1 2 3)
                 '((2 . nil) (4 . 2) (6 . 4))
                 (add-prev-reducer :transform (lambda (x) (* x 2)))
                 :from-end nil :initial-value nil)
  
  ;; Longer list
  (check-reducer "longer list: (10 20 30 40)" 
                 '(10 20 30 40)
                 '((10 . nil) (20 . 10) (30 . 20) (40 . 30))
                 (add-prev-reducer)
                 :from-end nil :initial-value nil))


;;; ---------------- Run All Tests ----------------

(defun test-all ()
  "Run all tests for Lab 4."
  (format t "~%")
  (format t "===============================================~%")
  (format t "  LAB 4: Higher-Order Functions and Closures  ~%")
  (format t "===============================================~%")
  
  ;; Part 1 tests
  (test-bubble-sort-functional-extended)
  (test-bubble-sort-imperative-extended)
  
  ;; Part 2 tests
  (test-add-prev-reducer)
  
  (format t "~%===============================================~%")
  (format t "  All tests completed!~%")
  (format t "===============================================~%"))