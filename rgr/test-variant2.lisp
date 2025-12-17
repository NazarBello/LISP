;;;; Тестові утиліти та набори тестів для варіанту №2

(load "variant2.lisp")

;;; Тестова утиліта
(defun test-equal (name expected actual &optional (tolerance 1e-6))
  "Перевіряє рівність очікуваного та фактичного значення з допустимою похибкою."
  (let ((diff (abs (- expected actual))))
    (if (<= diff tolerance)
        (format t "[PASS] ~A~%" name)
        (progn
          (format t "[FAIL] ~A~%" name)
          (format t "       Очікувалось: ~A~%" expected)
          (format t "       Отримано:    ~A~%" actual)
          (format t "       Різниця:     ~A~%" diff)))))


(defun test-error (name test-fn)
  "Перевіряє, чи функція викликає помилку."
  (handler-case
      (progn
        (funcall test-fn)
        (format t "[FAIL] ~A - помилка не виникла~%" name))
    (error (e)
      (format t "[PASS] ~A - помилка: ~A~%" name e))))


;;; Набір тестів 1: Базові значення
(defun test-base-values ()
  "Тестування базових значень F(1) та F(10)."
  (format t "~%=== Тест 1: Базові значення ===~%")
  (test-equal "F(1) має дорівнювати 1" 1 (get-value 1))
  (test-equal "F(10) має дорівнювати 2" 2 (get-value 10)))


;;; Набір тестів 2: Рекурсивне обчислення (2...9)
(defun test-recursive-2-9 ()
  "Тестування рекурсивних обчислень для діапазону i = 2...9."
  (format t "~%=== Тест 2: Рекурсивні обчислення (i = 2...9) ===~%")
  
  ;; F(2) = F(1) * 2 + 5*sin(2) = 1 * 2 + 5*sin(2)
  (let ((expected-f2 (+ (* 1 2) (* 5 (sin 2)))))
    (test-equal "F(2) обчислюється правильно" expected-f2 (get-value 2)))
  
  ;; F(3) = F(2) * 2 + 5*sin(3)
  (let* ((f2 (get-value 2))
         (expected-f3 (+ (* f2 2) (* 5 (sin 3)))))
    (test-equal "F(3) обчислюється правильно" expected-f3 (get-value 3)))
  
  ;; F(5) для перевірки проміжного значення
  (format t "F(5) = ~15,6F~%" (get-value 5)))


;;; Набір тестів 3: Рекурсивне обчислення (11...15)
(defun test-recursive-11-15 ()
  "Тестування рекурсивних обчислень для діапазону i = 11...15."
  (format t "~%=== Тест 3: Рекурсивні обчислення (i = 11...15) ===~%")
  
  ;; F(11) = F(10) * 2 + 5*sin(11) = 2 * 2 + 5*sin(11)
  (let ((expected-f11 (+ (* 2 2) (* 5 (sin 11)))))
    (test-equal "F(11) обчислюється правильно" expected-f11 (get-value 11)))
  
  ;; F(12) = F(11) * 2 + 5*sin(12)
  (let* ((f11 (get-value 11))
         (expected-f12 (+ (* f11 2) (* 5 (sin 12)))))
    (test-equal "F(12) обчислюється правильно" expected-f12 (get-value 12)))
  
  ;; F(15) для перевірки кінцевого значення першого діапазону
  (format t "F(15) = ~15,6F~%" (get-value 15)))


;;; Набір тестів 4: Рекурсивне обчислення (17...30)
(defun test-recursive-17-30 ()
  "Тестування рекурсивних обчислень для діапазону i = 17...30."
  (format t "~%=== Тест 4: Рекурсивні обчислення (i = 17...30) ===~%")
  
  ;; F(17) = F(16) / 2 + 5*cos(17) = F(15) / 2 + 5*cos(17)
  (let* ((f16 (get-value 16))
         (expected-f17 (+ (/ f16 2) (* 5 (cos 17)))))
    (test-equal "F(17) обчислюється правильно" expected-f17 (get-value 17)))
  
  ;; F(18) = F(17) / 2 + 5*cos(18)
  (let* ((f17 (get-value 17))
         (expected-f18 (+ (/ f17 2) (* 5 (cos 18)))))
    (test-equal "F(18) обчислюється правильно" expected-f18 (get-value 18)))
  
  ;; F(20) для перевірки проміжного значення
  (format t "F(20) = ~15,6F~%" (get-value 20))
  
  ;; F(30) для перевірки кінцевого значення
  (format t "F(30) = ~15,6F~%" (get-value 30)))


;;; Набір тестів 5: Порівняння рекурсивної та ітеративної версій
(defun test-recursive-vs-iterative ()
  "Порівняння результатів рекурсивної та ітеративної реалізацій."
  (format t "~%=== Тест 5: Порівняння рекурсивної та ітеративної версій ===~%")
  
  (let ((test-indices '(1 5 10 15 20 25 30)))
    (dolist (i test-indices)
      (let ((recursive (calculate-f i))
            (iterative (calculate-f-iterative i)))
        (test-equal (format nil "F(~D) - рекурсія vs ітерація" i)
                   recursive
                   iterative)))))


;;; Набір тестів 6: Граничні випадки та помилки
(defun test-edge-cases ()
  "Тестування граничних випадків та обробки помилок."
  (format t "~%=== Тест 6: Граничні випадки ===~%")
  
  (test-error "i = 0 викликає помилку"
             (lambda () (get-value 0)))
  
  (test-error "i = 31 викликає помилку"
             (lambda () (get-value 31)))
  
  (test-error "i = -5 викликає помилку"
             (lambda () (get-value -5)))
  
  (format t "F(1) = ~A (мінімальне допустиме значення)~%" (get-value 1))
  (format t "F(30) = ~A (максимальне допустиме значення)~%" (get-value 30)))


;;; Набір тестів 7: Математична коректність
(defun test-mathematical-correctness ()
  "Перевірка математичної коректності обчислень."
  (format t "~%=== Тест 7: Математична коректність ===~%")
  
  ;; Перевірка, що F(2) = F(1)*2 + 5*sin(2)
  (let* ((f1 (get-value 1))
         (f2 (get-value 2))
         (expected (+ (* f1 2) (* 5 (sin 2)))))
    (test-equal "Формула для F(2) правильна" expected f2))
  
  ;; Перевірка, що F(17) = F(16)/2 + 5*cos(17)
  (let* ((f16 (get-value 16))
         (f17 (get-value 17))
         (expected (+ (/ f16 2) (* 5 (cos 17)))))
    (test-equal "Формула для F(17) правильна" expected f17))
  
  ;; Перевірка монотонності в першому діапазоні (2-15)
  (format t "Перевірка зростання в діапазоні 2-9:~%")
  (loop for i from 2 to 8
        for current = (get-value i)
        for next = (get-value (+ i 1))
        do (if (< current next)
               (format t "  F(~D) < F(~D) ✓~%" i (+ i 1))
               (format t "  F(~D) >= F(~D) (можлива аномалія)~%" i (+ i 1)))))


;;; Головна функція запуску всіх тестів
(defun run-all-tests ()
  "Запускає всі набори тестів."
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  ТЕСТУВАННЯ ПРОГРАМИ - ВАРІАНТ №2                ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")
  
  (test-base-values)
  (test-recursive-2-9)
  (test-recursive-11-15)
  (test-recursive-17-30)
  (test-recursive-vs-iterative)
  (test-edge-cases)
  (test-mathematical-correctness)
  
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  ТЕСТУВАННЯ ЗАВЕРШЕНО                            ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%"))


;;; Функція для виведення таблиці значень
(defun print-full-table ()
  "Виводить повну таблицю всіх значень F(i) від 1 до 30."
  (format t "~%╔═══════╦═══════════════════╗~%")
  (format t "║   i   ║      F(i)         ║~%")
  (format t "╠═══════╬═══════════════════╣~%")
  (loop for i from 1 to 30 do
    (format t "║  ~2D   ║ ~15,6F  ║~%" i (get-value i)))
  (format t "╚═══════╩═══════════════════╝~%"))