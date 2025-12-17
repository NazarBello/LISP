#!/usr/bin/env python3


import math


def calculate_f(i, cache=None):
    """Обчислює F(i) згідно з варіантом №2"""
    if cache is None:
        cache = {}
    
    # Базові випадки
    if i == 1:
        return 1
    if i == 10:
        return 2
    
    # Перевірка кешу
    if i in cache:
        return cache[i]
    
    # Рекурсивні обчислення
    if 2 <= i <= 9:
        result = calculate_f(i-1, cache) * 2 + 5 * math.sin(i)
    elif 11 <= i <= 15:
        result = calculate_f(i-1, cache) * 2 + 5 * math.sin(i)
    elif i == 16:
        result = calculate_f(15, cache)
    elif 17 <= i <= 30:
        result = calculate_f(i-1, cache) / 2 + 5 * math.cos(i)
    else:
        raise ValueError(f"i має бути в діапазоні 1-30")
    
    cache[i] = result
    return result


# Виведення таблиці
print("╔═══════╦═══════════════════════╗")
print("║   i   ║        F(i)           ║")
print("╠═══════╬═══════════════════════╣")

cache = {}
for i in range(1, 31):
    result = calculate_f(i, cache)
    print(f"║  {i:2d}   ║ {result:19.10f}  ║")

print("╚═══════╩═══════════════════════╝")