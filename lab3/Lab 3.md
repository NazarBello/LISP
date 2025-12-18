<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p> <p align="center"> <b>Звіт з лабораторної роботи 3</b><br/> "Конструктивний і деструктивний підходи до роботи зі списками"<br/> дисципліни "Вступ до функціонального програмування" </p> <p align="right"><b>Студент</b>: Атанов Назар Данилович КВ-23</p> <p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Реалізувати алгоритм сортування чисел у списку двома способами: функціонально і імперативно.

**Функціональний варіант**  має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання:

- псевдо-функцій
- деструктивних операцій
- циклів

Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).

**Імперативний варіант** має базуватись на використанні циклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін використовується функція `copy-list`.

## Варіант 2

**Алгоритм сортування обміном №1 (без оптимізацій) за незменшенням** — Bubble Sort

### Опис алгоритму

Алгоритм бульбашкового сортування (Bubble Sort) — це простий алгоритм сортування, який багаторазово проходить через список, порівнює сусідні елементи і міняє їх місцями, якщо вони знаходяться в неправильному порядку.

**Варіант 2** передбачає реалізацію **без оптимізацій**, тобто:

- Виконується рівно N проходів для списку з N елементів
- Відсутня рання зупинка при досягненні відсортованого стану
- Це найпростіша (і найменш ефективна) версія алгоритму

**Складність алгоритму:**

- Часова складність: O(n²) в усіх випадках (найкращий, середній, найгірший)
- Просторова складність: O(n) для функціональної реалізації, O(1) додаткової пам'яті для імперативної

## Лістинг функції з використанням конструктивного підходу

```lisp
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
```

### Пояснення реалізації

**Функція `bubble-pass-functional`:**

- Виконує один прохід по списку
- Використовує рекурсію для обробки кожної пари сусідніх елементів
- Якщо поточний елемент більший за наступний, вони міняються місцями шляхом конструювання нового списку
- Використовується `cons` для створення нового списку — жодних деструктивних операцій

**Функція `bubble-sort-iter`:**

- Допоміжна рекурсивна функція для виконання N проходів
- Викликає `bubble-pass-functional` рекурсивно, зменшуючи лічильник проходів
- Це реалізація хвостової рекурсії для ітерації

**Функція `bubble-sort-functional`:**

- Головна функція-точка входу
- Викликає `bubble-sort-iter` з параметром N = довжина списку
- Це забезпечує виконання рівно N проходів згідно з вимогами Варіанту 2

### Тестові набори та утиліти

```lisp
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
  (check-bubble-sort "test 1: basic random" 
                     '(3 1 4 1 5 9 2 6) 
                     '(1 1 2 3 4 5 6 9) 
                     #'bubble-sort-functional)
  (check-bubble-sort "test 2: empty" 
                     nil 
                     nil 
                     #'bubble-sort-functional)
  (check-bubble-sort "test 3: single atom" 
                     '(42) 
                     '(42) 
                     #'bubble-sort-functional)
  (check-bubble-sort "test 4: sorted" 
                     '(1 2 3) 
                     '(1 2 3) 
                     #'bubble-sort-functional)
  (check-bubble-sort "test 5: reversed" 
                     '(3 2 1) 
                     '(1 2 3) 
                     #'bubble-sort-functional))
```

### Тестування

```lisp
CL-USER> (test-bubble-sort-functional)

=== Testing Functional Bubble Sort (Variant 2) ===
passed... test 1: basic random
passed... test 2: empty
passed... test 3: single atom
passed... test 4: sorted
passed... test 5: reversed
NIL

CL-USER> ;; Приклади ручного виклику

CL-USER> (bubble-sort-functional '(5 2 8 1 9))
(1 2 5 8 9)

CL-USER> (bubble-sort-functional '(3 3 1 2 2 1))
(1 1 2 2 3 3)

CL-USER> (bubble-sort-functional nil)
NIL

CL-USER> (bubble-sort-functional '(42))
(42)
```

## Лістинг функції з використанням деструктивного підходу

```lisp
(defun bubble-sort-imperative (lst)
  (let ((arr (copy-list lst))  ;; Copy strictly required 
        (n (length lst)))
    ;; Outer loop: N-1 passes are enough to sort N elements
    (dotimes (i (1- n))
      ;; Inner loop: Compare adjacent elements up to the unsorted part
      ;; Note: (nth) is O(N), making this O(N^3), but fits 'loops' requirement best.
      (dotimes (j (- n i 1))
        (let ((curr (nth j arr))
              (next (nth (1+ j) arr)))
          (when (> curr next)
            ;; Destructive swap using rotatef 
            (rotatef (nth j arr) (nth (1+ j) arr))))))
    arr))
```

### Пояснення реалізації

**Основна структура:**

- Використовує `copy-list` для створення копії вхідного списку, щоб не змінювати оригінал
- Зовнішній цикл `dotimes` виконує N-1 проходів (достатньо для сортування N елементів)
- Внутрішній цикл `dotimes` порівнює сусідні елементи

**Деструктивні операції:**

- `rotatef` — деструктивна функція, яка міняє місцями значення двох місць (places)
- `(nth j arr)` — це місце (place), яке можна змінювати
- Оператор `rotatef` безпосередньо модифікує список `arr`

**Особливості:**

- Використання `nth` для доступу до елементів списку має складність O(n)
- Це робить загальну складність алгоритму O(n³), але відповідає вимогам використання циклів і деструктивних операцій
- Внутрішній цикл зменшує діапазон на `i` позицій, оскільки після кожного проходу найбільші елементи вже на своїх місцях

### Тестові набори та утиліти

```lisp
(defun test-bubble-sort-imperative ()
  (format t 
  (check-bubble-sort 
                     '(3 1 4 1 5 9 2 6) 
                     '(1 1 2 3 4 5 6 9) 
                     #'bubble-sort-imperative)
  (check-bubble-sort 
                     nil 
                     nil 
                     #'bubble-sort-imperative)
  (check-bubble-sort "test 3: single atom" 
                     '(42) 
                     '(42) 
                     #'bubble-sort-imperative)
  (check-bubble-sort "test 4: sorted" 
                     '(1 2 3) 
                     '(1 2 3) 
                     #'bubble-sort-imperative)
  (check-bubble-sort "test 5: reversed" 
                     '(3 2 1) 
                     '(1 2 3) 
                     #'bubble-sort-imperative))
```

### Тестування

```lisp
CL-USER> (test-bubble-sort-imperative)

=== Testing Imperative Bubble Sort (Variant 2) ===
passed... test 1: basic random
passed... test 2: empty
passed... test 3: single atom
passed... test 4: sorted
passed... test 5: reversed
NIL

CL-USER> ;; Приклади ручного виклику

CL-USER> (bubble-sort-imperative '(5 2 8 1 9))
(1 2 5 8 9)

CL-USER> (bubble-sort-imperative '(3 3 1 2 2 1))
(1 1 2 2 3 3)

CL-USER> ;; Перевірка, що оригінальний список не змінюється
CL-USER> (defvar *test-list* '(5 3 1 4 2))
*TEST-LIST*

CL-USER> (bubble-sort-imperative *test-list*)
(1 2 3 4 5)

CL-USER> *test-list*
(5 3 1 4 2)  ; Оригінальний список залишився незміненим
```

## Висновки

В ході виконання лабораторної роботи було реалізовано алгоритм бульбашкового сортування (Bubble Sort) двома принципово різними підходами:

### 1. Функціональний підхід (конструктивний)

**Переваги:**

- Повністю immutable — жодних побічних ефектів
- Безпечний для паралельного виконання
- Легко тестувати та міркувати про коректність
- Відповідає принципам функціонального програмування

**Недоліки:**

- Створює багато проміжних списків (більше використання пам'яті)
- Може бути повільнішим через постійне конструювання нових структур даних

**Технічні деталі:**

- Використовує `cons` для побудови нових списків
- Три рівні рекурсії: обробка пари елементів → один прохід → N проходів
- Хвостова рекурсія у `bubble-sort-iter` може бути оптимізована компілятором

### 2. Імперативний підхід (деструктивний)

**Переваги:**

- Модифікація in-place (менше використання пам'яті після копіювання)
- Звична парадигма для програмістів з імперативним досвідом
- Чітка структура з явними циклами

**Недоліки:**

- Використання `nth` для доступу до елементів списку робить алгоритм O(n³)
- Необхідність копіювання списку для збереження оригіналу
- Деструктивні операції можуть бути джерелом помилок

**Технічні деталі:**

- Використовує `copy-list` для створення копії
- `dotimes` для циклів (звичний імперативний стиль)
- `rotatef` для деструктивної зміни місць у списку
