;;;; Розрахунково-графічна робота
;;;; Варіант №2
;;;; Обчислення рекурсивної функції F(i)

;;; Базові значення
;;; F₁ = 1, F₁₀ = 2
;;;
;;; Рекурсивна формула:
;;; F(i) = F(i-1) · 2 + 5sin(i),     i = 2...15
;;; F(i) = F(i-1) / 2 + 5cos(i),     i = 17...30

(defun calculate-f (i)
  "Обчислює значення функції F для заданого індексу i.
   Використовує рекурсивний підхід відповідно до варіанту №2."
  (cond
    ;; Базові випадки
    ((= i 1) 1)
    ((= i 10) 2)
    
    ;; Рекурсивні випадки для i = 2...9
    ((and (>= i 2) (<= i 9))
     (+ (* (calculate-f (- i 1)) 2)
        (* 5 (sin i))))
    
    ;; Рекурсивні випадки для i = 11...15
    ((and (>= i 11) (<= i 15))
     (+ (* (calculate-f (- i 1)) 2)
        (* 5 (sin i))))
    
    ;; Базовий випадок для переходу від 16 до 17
    ;; Використовуємо F(15) для обчислення F(17)
    ((= i 16) (calculate-f 15))  ; Проміжне значення
    
    ;; Рекурсивні випадки для i = 17...30
    ((and (>= i 17) (<= i 30))
     (+ (/ (calculate-f (- i 1)) 2)
        (* 5 (cos i))))
    
    ;; Випадок помилки
    (t (error "Індекс i повинен бути в діапазоні 1-30, отримано: ~A" i))))


(defun calculate-f-iterative (i)
  "Ітеративна версія обчислення функції F для оптимізації.
   Використовує кешування проміжних результатів."
  (if (or (< i 1) (> i 30))
      (error "Індекс i повинен бути в діапазоні 1-30, отримано: ~A" i)
      (let ((cache (make-hash-table)))
        ;; Ініціалізація базових значень
        (setf (gethash 1 cache) 1)
        (setf (gethash 10 cache) 2)
        
        ;; Обчислення значень від 2 до 9
        (loop for idx from 2 to 9 do
          (setf (gethash idx cache)
                (+ (* (gethash (- idx 1) cache) 2)
                   (* 5 (sin idx)))))
        
        ;; Обчислення значень від 11 до 15
        (loop for idx from 11 to 15 do
          (setf (gethash idx cache)
                (+ (* (gethash (- idx 1) cache) 2)
                   (* 5 (sin idx)))))
        
        ;; Проміжне значення для 16
        (setf (gethash 16 cache) (gethash 15 cache))
        
        ;; Обчислення значень від 17 до 30
        (loop for idx from 17 to 30 do
          (setf (gethash idx cache)
                (+ (/ (gethash (- idx 1) cache) 2)
                   (* 5 (cos idx)))))
        
        (gethash i cache))))


(defun print-sequence (start end)
  "Виводить послідовність значень F(i) від start до end."
  (format t "~%Послідовність значень F(i):~%")
  (format t "~%---------------------------------~%")
  (loop for i from start to end do
    (format t "F(~2D) = ~15,6F~%" i (calculate-f-iterative i)))
  (format t "---------------------------------~%"))


(defun get-value (i)
  "Зручна функція для отримання одного значення F(i)."
  (calculate-f-iterative i))


;;; Приклад використання
(defun demo ()
  "Демонстрація роботи програми."
  (format t "~%=== Розрахунково-графічна робота. Варіант №2 ===~%")
  
  ;; Виведення базових значень
  (format t "~%Базові значення:~%")
  (format t "F(1)  = ~A~%" (get-value 1))
  (format t "F(10) = ~A~%" (get-value 10))
  
  ;; Виведення першого діапазону
  (print-sequence 2 15)
  
  ;; Виведення другого діапазону
  (print-sequence 17 30)
  
  ;; Виведення окремих значень для перевірки
  (format t "~%Контрольні значення:~%")
  (format t "F(5)  = ~15,6F~%" (get-value 5))
  (format t "F(15) = ~15,6F~%" (get-value 15))
  (format t "F(20) = ~15,6F~%" (get-value 20))
  (format t "F(30) = ~15,6F~%" (get-value 30)))