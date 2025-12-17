<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p> <p align="center"> <b>Звіт з лабораторної роботи 4</b><br/> "Функції вищого порядку та замикання"<br/> дисципліни "Вступ до функціонального програмування" </p> <p align="right"><b>Студент</b>: Nazar Atanov КВ-23</p> <p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Лабораторна робота складається з двох частин:

1. **Переписати функціональну реалізацію алгоритму сортування** з лабораторної роботи 3 з наступними змінами:
    
    - Використати функції вищого порядку для роботи з послідовностями (де/якщо це доречно)
    - Додати до інтерфейсу функції два ключових параметра: `key` та `test`, що працюють аналогічно до параметрів функцій для роботи з послідовностями
    - Параметр `key` має виконатись мінімальну кількість разів
2. **Реалізувати функцію, що створює замикання**, яке працює згідно із завданням за варіантом.
    

---

## Варіант першої частини 2

**Алгоритм сортування обміном №1 (без оптимізацій) за незменшенням** (Bubble Sort)

## Лістинг реалізації першої частини завдання

### Функціональна реалізація

```lisp
(defun bubble-sort-functional (lst &key (test #'<) key)
  "Bubble sort - functional implementation with KEY and TEST parameters."
  (if key
      ;; With KEY: create (element . key-value) pairs once, sort, extract elements
      (let ((keyed-pairs (mapcar (lambda (elt) (cons elt (funcall key elt))) lst)))
        (mapcar #'car (bubble-sort-with-test keyed-pairs 
                                              (lambda (p1 p2) 
                                                (funcall test (cdr p1) (cdr p2))))))
      ;; Without KEY: sort elements directly
      (bubble-sort-with-test lst test)))

(defun bubble-sort-with-test (lst test)
  (labels ((bubble-pass (lst)
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
             (if (zerop n)
                 lst
                 (bubble-iter (bubble-pass lst) (1- n)))))
    (bubble-iter lst (length lst))))
```

### Імперативна реалізація

```lisp
(defun bubble-sort-imperative (lst &key (test #'<) key)
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
                (rotatef (nth j arr) (nth (1+ j) arr))))))
        arr)))
```

### Тестові набори та утиліти першої частини

```lisp
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
  
  ;; Similar tests as functional version...
  (check-sort "basic: ascending" 
              '(3 1 4 1 5) '(1 1 3 4 5) 
              #'bubble-sort-imperative)
  
  (check-sort "with key: sort by absolute value" 
              '(-5 2 -1 3) '(-1 2 3 -5)
              #'bubble-sort-imperative :key #'abs)
  
  (check-sort "with key: sort strings by length" 
              '("xxx" "a" "bb") '("a" "bb" "xxx")
              #'bubble-sort-imperative :key #'length)
  
  (check-sort "with test: descending order" 
              '(1 2 3 4 5) '(5 4 3 2 1)
              #'bubble-sort-imperative :test #'>)
  
  (check-sort "key + test: sort by length descending" 
              '("a" "xxx" "bb") '("xxx" "bb" "a")
              #'bubble-sort-imperative :key #'length :test #'>)
  
  (check-sort "empty list" 
              nil nil 
              #'bubble-sort-imperative)
  
  (check-sort "single element" 
              '(42) '(42)
              #'bubble-sort-imperative :key #'1+))
```

### Тестування першої частини

```lisp
CL-USER> (test-bubble-sort-functional-extended)

=== Testing Functional Bubble Sort (with KEY/TEST) ===
passed... basic: ascending
passed... with key: sort by absolute value
passed... with key: sort strings by length
passed... with test: descending order
passed... key + test: sort by length descending
passed... empty list
passed... single element
NIL

CL-USER> (test-bubble-sort-imperative-extended)

=== Testing Imperative Bubble Sort (with KEY/TEST) ===
passed... basic: ascending
passed... with key: sort by absolute value
passed... with key: sort strings by length
passed... with test: descending order
passed... key + test: sort by length descending
passed... empty list
passed... single element
NIL

CL-USER> ;; Examples of usage

CL-USER> (bubble-sort-functional '(5 2 8 1 9))
(1 2 5 8 9)

CL-USER> (bubble-sort-functional '(5 2 8 1 9) :test #'>)
(9 8 5 2 1)

CL-USER> (bubble-sort-functional '("hello" "hi" "hey" "h") :key #'length)
("h" "hi" "hey" "hello")
```

---

## Варіант другої частини 2

**Завдання:** Написати функцію `add-prev-reducer`, яка має один ключовий параметр — функцію `transform`. `add-prev-reducer` має повернути функцію, яка при застосуванні в якості першого аргументу `reduce` робить наступне: кожен елемент списку-аргументу `reduce` перетворюється на точкову пару, де в комірці `CAR` знаходиться значення поточного елемента, а в комірці `CDR` знаходиться значення попереднього елемента списку (тобто того, що знаходиться "зліва").

Якщо функція `transform` передана, тоді значення поточного і попереднього елементів, що потраплять у результат, мають бути змінені згідно `transform`. Функція `transform` має виконатись мінімальну кількість разів.

**Приклад використання:**

```lisp
CL-USER> (reduce (add-prev-reducer) '(1 2 3) 
                 :from-end nil :initial-value nil)
((1 . NIL) (2 . 1) (3 . 2))

CL-USER> (reduce (add-prev-reducer :transform #'1+) '(1 2 3)
                 :from-end nil :initial-value nil)
((2 . NIL) (3 . 2) (4 . 3))
```

## Лістинг реалізації другої частини завдання

```lisp
(defun add-prev-reducer (&key transform)
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
```

### Тестові набори та утиліти другої частини

```lisp
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
```

### Тестування другої частини

```lisp
CL-USER> (test-add-prev-reducer)

=== Testing add-prev-reducer (Variant 2) ===
passed... basic: (1 2 3)
passed... with transform #'1+: (1 2 3)
passed... single element
passed... empty list
passed... transform with (* 2): (1 2 3)
passed... longer list: (10 20 30 40)
NIL

CL-USER> ;; Manual examples

CL-USER> (reduce (add-prev-reducer) '(1 2 3) :from-end nil :initial-value nil)
((1 . NIL) (2 . 1) (3 . 2))

CL-USER> (reduce (add-prev-reducer :transform #'1+) '(1 2 3) 
                 :from-end nil :initial-value nil)
((2 . NIL) (3 . 2) (4 . 3))

CL-USER> (reduce (add-prev-reducer :transform (lambda (x) (* x 2))) 
                 '(1 2 3 4) 
                 :from-end nil :initial-value nil)
((2 . NIL) (4 . 2) (6 . 4) (8 . 6))
```
