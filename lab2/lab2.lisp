

(defun remove-seconds-and-thirds (lst)
  "Removes every second and third element from the list.
    Uses recursion with a helper function to track position."
  (remove-seconds-and-thirds-helper lst 1))

(defun remove-seconds-and-thirds-helper (lst position)
  "Helper function that tracks the current position in the list.
   Keeps elements at positions 1, 4, 7, 10, ... (i.e., position mod 3 = 1)"
  (cond
    ((null lst) nil)  
    ((= (mod position 3) 1)  
     (cons (car lst) 
            (remove-seconds-and-thirds-helper (cdr lst) (+ position 1))))
    (t  
     (remove-seconds-and-thirds-helper (cdr lst) (+ position 1)))))



(defun list-set-intersection (set1 set2)
  "Returns the intersection of two sets represented as lists of atoms.
   Uses recursion to check each element of set1 against set2."
  (cond
    ((null set1) nil) 
    ((member-helper (car set1) set2)  
     (cons (car set1) 
           (list-set-intersection (cdr set1) set2)))
    (t  
     (list-set-intersection (cdr set1) set2))))

(defun member-helper (element lst)
  "Helper function to check if element is in the list.
   Returns T if found, NIL otherwise."
  (cond
    ((null lst) nil)  
    ((eql element (car lst)) t) 
    (t (member-helper element (cdr lst)))))  


;;; ====================================================================
;;; Test Utilities
;;; ====================================================================

(defun check-result (test-name actual expected)
  "Compares actual result with expected and prints status."
  (format t "~:[FAILED~;PASSED~] ... ~a~%" 
          (equal actual expected) 
          test-name)
  (when (not (equal actual expected))
    (format t "  Expected: ~a~%" expected)
    (format t "  Got:      ~a~%" actual)))


;;; ====================================================================
;;; Test Cases
;;; ====================================================================

(defun run-all-tests ()
  "Run all test suites for both functions."
  (format t "~%")
  (format t "========================================~%")
  (format t "    LAB 2 - VARIANT 2 TEST SUITE~%")
  (format t "========================================~%")
  
  (format t "~%Testing remove-seconds-and-thirds:~%")
  (format t "===================================~%")
  
  (check-result "Test 1: (a b c d e f g)"
                (remove-seconds-and-thirds '(a b c d e f g))
                '(a d g))
  
  (check-result "Test 2: (1 2 3 4 5 6 7 8 9)"
                (remove-seconds-and-thirds '(1 2 3 4 5 6 7 8 9))
                '(1 4 7))
  
  (check-result "Test 3: empty list"
                (remove-seconds-and-thirds nil)
                nil)
  
  (check-result "Test 4: (a)"
                (remove-seconds-and-thirds '(a))
                '(a))
  
  (check-result "Test 5: (a b)"
                (remove-seconds-and-thirds '(a b))
                '(a))
  
  (check-result "Test 6: (a b c)"
                (remove-seconds-and-thirds '(a b c))
                '(a))
  
  (check-result "Test 7: (a b c d)"
                (remove-seconds-and-thirds '(a b c d))
                '(a d))
  
  (check-result "Test 8: (1 2 3 4 5 6 7 8 9 10 11 12)"
                (remove-seconds-and-thirds '(1 2 3 4 5 6 7 8 9 10 11 12))
                '(1 4 7 10))

  (format t "~%Testing list-set-intersection:~%")
  (format t "================================~%")
  
  (check-result "Test 1: (1 2 3 4) ∩ (3 4 5 6)"
                (list-set-intersection '(1 2 3 4) '(3 4 5 6))
                '(3 4))
  
  (check-result "Test 2: (1 2 3) ∩ (4 5 6)"
                (list-set-intersection '(1 2 3) '(4 5 6))
                nil)
  
  (check-result "Test 3: (1 2 3) ∩ (1 2 3)"
                (list-set-intersection '(1 2 3) '(1 2 3))
                '(1 2 3))
  
  (check-result "Test 4: () ∩ (1 2 3)"
                (list-set-intersection nil '(1 2 3))
                nil)
  
  (check-result "Test 5: (1 2 3) ∩ ()"
                (list-set-intersection '(1 2 3) nil)
                nil)
  
  (check-result "Test 6: () ∩ ()"
                (list-set-intersection nil nil)
                nil)
  
  (check-result "Test 7: (a b c) ∩ (b c d)"
                (list-set-intersection '(a b c) '(b c d))
                '(b c))
  
  (check-result "Test 8: (1 2 3 4 5) ∩ (3)"
                (list-set-intersection '(1 2 3 4 5) '(3))
                '(3))
  
  (check-result "Test 9: (1 2) ∩ (1 2 3 4)"
                (list-set-intersection '(1 2) '(1 2 3 4))
                '(1 2))
  
  (format t "~%========================================~%")
  (format t "           TESTS COMPLETED~%")
  (format t "========================================~%")
  (format t "~%"))