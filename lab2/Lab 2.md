<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p> <p align="center"> <b>Звіт з лабораторної роботи 2</b><br/> "Рекурсія"<br/> дисципліни "Вступ до функціонального програмування" </p> <p align="right"><b>Студент(-ка)</b>: Атанов Назар Данилович КВ-23</p> <p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

 Реалізувати дві рекурсивні функції, що виконують операції з вхідними списками, використовуючи різні види рекурсії. Функції мають конструювати нові списки, а не змінювати вхідні. Заборонено використовувати функції вищого порядку, стандартні функції (крім базових з розділу 4), цикли та деструктивні операції.

## Варіант 2

### Завдання 1

Написати функцію  `remove-seconds-and-thirds`, яка видаляє зі списку кожен другий і третій елементи.

**Приклад:**

```lisp
CL-USER> (remove-seconds-and-thirds '(a b c d e f g))
(A D G)
```

### Завдання 2

Написати функцію `list-set-intersection`, яка визначає перетин двох множин, заданих списками атомів.

**Приклад:**

```lisp
CL-USER> (list-set-intersection '(1 2 3 4) '(3 4 5 6))
(3 4) ; порядок може відрізнятись
```

## Лістинг функції `remove-seconds-and-thirds`

```lisp
(defun remove-seconds-and-thirds (lst)
  "Removes every second and third element from the list.
   Uses recursion with a helper function to track position."
  (remove-seconds-and-thirds-helper lst 1))

(defun remove-seconds-and-thirds-helper (lst position)
  "Helper function that tracks the current position in the list.
   Keeps elements at positions 1, 4, 7, 10, ... (i.e., position mod 3 = 1)"
  (cond
    ((null lst) nil)  ; Base case: empty list
    ((= (mod position 3) 1)  ; Keep element if position mod 3 = 1
     (cons (car lst) 
           (remove-seconds-and-thirds-helper (cdr lst) (+ position 1))))
    (t  ; Skip element if position mod 3 = 2 or 0
     (remove-seconds-and-thirds-helper (cdr lst) (+ position 1)))))
```

**Пояснення:** Функція використовує допоміжну рекурсивну функцію з лічильником позиції. На кожному кроці рекурсії:

- Якщо список порожній, повертаємо `NIL` (базовий випадок)
- Якщо позиція mod 3 дорівнює 1 (позиції 1, 4, 7, ...), зберігаємо елемент
- Інакше (позиції 2, 3, 5, 6, 8, 9, ...), пропускаємо елемент

Це хвостова рекурсія з накопиченням через `CONS`.

### Тестові набори та утиліти

```lisp
(defun check-result (test-name actual expected)
  "Compares actual result with expected and prints status."
  (format t "~:[FAILED~;PASSED~] ... ~a~%" 
          (equal actual expected) 
          test-name)
  (when (not (equal actual expected))
    (format t "  Expected: ~a~%" expected)
    (format t "  Got:      ~a~%" actual)))

(defun run-all-tests ()
  "Run all test suites for both functions."
  (format t "~%")
  (format t "========================================~%")
  (format t "    LAB 2 - VARIANT 2 TEST SUITE~%")
  (format t "========================================~%")
  
  ;; Tests for remove-seconds-and-thirds
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
  
  ;; Tests for list-set-intersection
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
```

### Тестування

```lisp
[2]> (run-all-tests)

========================================
    LAB 2 - VARIANT 2 TEST SUITE
========================================

Testing remove-seconds-and-thirds:
===================================
PASSED ... Test 1: (a b c d e f g)
PASSED ... Test 2: (1 2 3 4 5 6 7 8 9)
PASSED ... Test 3: empty list
PASSED ... Test 4: (a)
PASSED ... Test 5: (a b)
PASSED ... Test 6: (a b c)
PASSED ... Test 7: (a b c d)
PASSED ... Test 8: (1 2 3 4 5 6 7 8 9 10 11 12)

Testing list-set-intersection:
================================
PASSED ... Test 1: (1 2 3 4) ∩ (3 4 5 6)
PASSED ... Test 2: (1 2 3) ∩ (4 5 6)
PASSED ... Test 3: (1 2 3) ∩ (1 2 3)
PASSED ... Test 4: () ∩ (1 2 3)
PASSED ... Test 5: (1 2 3) ∩ ()
PASSED ... Test 6: () ∩ ()
PASSED ... Test 7: (a b c) ∩ (b c d)
PASSED ... Test 8: (1 2 3 4 5) ∩ (3)
PASSED ... Test 9: (1 2) ∩ (1 2 3 4)

========================================
           TESTS COMPLETED
========================================

NIL
```

## Лістинг функції `list-set-intersection`

```lisp
(defun list-set-intersection (set1 set2)
  "Returns the intersection of two sets represented as lists of atoms.
   Uses recursion to check each element of set1 against set2."
  (cond
    ((null set1) nil)  ; Base case: empty first set
    ((member-helper (car set1) set2)  ; If first element is in set2
     (cons (car set1) 
           (list-set-intersection (cdr set1) set2)))
    (t  ; First element not in set2, skip it
     (list-set-intersection (cdr set1) set2))))

(defun member-helper (element lst)
  "Helper function to check if element is in the list.
   Returns T if found, NIL otherwise."
  (cond
    ((null lst) nil)  ; Base case: element not found
    ((eql element (car lst)) t)  ; Found the element
    (t (member-helper element (cdr lst)))))  ; Keep searching
```

**Пояснення:** Функція використовує рекурсію для перебору першої множини:

- Базовий випадок: якщо перша множина порожня, повертаємо `NIL`
- Якщо перший елемент `set1` знаходиться в `set2` (перевіряємо через `member-helper`), додаємо його до результату
- Інакше пропускаємо елемент і продовжуємо з рештою списку

Допоміжна функція `member-helper` рекурсивно шукає елемент у списку, не використовуючи стандартну функцію `MEMBER`.

Це звичайна рекурсія з конструюванням нового списку через `CONS`.

## Висновки

У ході виконання лабораторної роботи було:

1. Реалізовано дві рекурсивні функції відповідно до варіанту завдання
2. Використано різні види рекурсії:
    - Хвостова рекурсія з лічильником для `remove-seconds-and-thirds`
    - Звичайна рекурсія з конструюванням списку для `list-set-intersection`
3. Створено допоміжні функції для підтримки основної логіки
4. Розроблено комплексні тестові набори з перевіркою граничних випадків
5. Всі функції дотримуються вимог завдання: не використовують циклів, деструктивних операцій, функцій вищого порядку
6. Отримано практичні навички роботи з рекурсією в функціональному програмуванні

Обидві функції працюють коректно, що підтверджується успішним проходженням усіх тестів.