
# Comparación de Paradigmas: Python (Imperativo) vs Haskell (Funcional)

## 1. Introducción

Este informe compara los enfoques imperativo (Python) y funcional (Haskell) aplicados al problema de ordenar una lista de estudiantes según su calificación y nombre. Se analizarán aspectos clave como claridad, expresividad, manejo de estructuras de datos, estado, mantenimiento y eficiencia.

---

## 2. Comparación de claridad y legibilidad

### Python (Imperativo)

- **Ventaja:** Describe explícitamente el proceso de ordenamiento usando bucles anidados y comparaciones.
- **Desventaja:** Puede volverse verboso y propenso a errores debido a la manipulación directa de variables.

```python
for i in range(n - 1):
    for j in range(n - 1 - i):
        if (estudiantes[j][1] < estudiantes[j + 1][1]) or \
           (estudiantes[j][1] == estudiantes[j + 1][1] and estudiantes[j][0] > estudiantes[j + 1][0]):
            estudiantes[j], estudiantes[j + 1] = estudiantes[j + 1], estudiantes[j]
```

### Haskell (Funcional)

- **Ventaja:** Expresa el ordenamiento de manera declarativa, sin detallar los pasos intermedios.
- **Desventaja:** Puede ser menos intuitivo para quienes no están familiarizados con funciones de orden superior.

```haskell
sortBy (comparing (\(nombre, nota) -> (Down nota, nombre)))
```

**Conclusión:** Python es más explícito, mientras que Haskell es más conciso y declarativo.

---

## 3. Expresividad y abstracción

### Python

- **Abstracción baja:** Se requieren instrucciones detalladas para manejar los índices y realizar swaps manualmente.
- **Ejemplo:** Implementa el ordenamiento con lógica condicional explícita.

### Haskell

- **Abstracción alta:** El criterio de ordenamiento se expresa mediante una función.
- **Ejemplo:** `Down nota` invierte el orden numérico, y `nombre` ordena alfabéticamente sin bucles ni variables temporales.

**Conclusión:** Haskell permite mayor expresividad, eliminando detalles de implementación.

---

## 4. Manejo de estructuras de datos

### Python (Mutabilidad)

- La lista original se modifica *in-place*, lo que puede afectar otras partes del programa.
- **Riesgo:** Puede generar efectos secundarios no deseados.

### Haskell (Inmutabilidad)

- Se genera una nueva lista ordenada sin modificar la original.
- **Ventaja:** Reduce riesgos de efectos colaterales y mejora la seguridad del código.

**Conclusión:** La inmutabilidad en Haskell garantiza mayor seguridad, mientras que la mutabilidad en Python permite optimización de memoria.

---

## 5. Manejo de estado

### Python

- **Estado explícito:** Se usan variables como `i`, `j` y la lista `estudiantes` que cambian durante la ejecución.
- **Ejemplo:** Los bucles anidados controlan el ordenamiento.

### Haskell

- **Sin estado mutable:** Se usa composición de funciones sin modificar datos.
- **Ejemplo:** `sortBy` devuelve una nueva lista sin cambiar la original.

**Conclusión:** Haskell evita la complejidad del manejo de estado, mientras que Python lo necesita para el paradigma imperativo.

---

## 6. Facilidad de mantenimiento y extensión

### Python

- **Mantenimiento:** Cambiar el criterio de ordenamiento requiere modificar varias condiciones anidadas.
- **Riesgo:** Puede introducir errores al modificar la lógica.

### Haskell

- **Extensión:** Agregar un nuevo criterio de orden es tan simple como modificar la tupla en `comparing`.
- **Ejemplo:** Agregar un tercer criterio (edad) solo requiere extender la función.

```haskell
comparing (\(nombre, nota, edad) -> (Down nota, nombre, edad))
```

**Conclusión:** Haskell facilita la extensión gracias a su enfoque declarativo.

---

## 7. Eficiencia

### Python (Bubble Sort)

- **Complejidad:** O(n²), ineficiente para listas grandes.
- **Ejemplo:** 100 elementos requieren \~10,000 operaciones.

### Haskell (sortBy)

- **Complejidad:** O(n log n) usando algoritmos eficientes como Merge Sort.
- **Ventaja:** Escalable para grandes volúmenes de datos.

**Conclusión:** Haskell es más eficiente en problemas de gran escala, mientras que Python podría optimizarse con `sorted()`.

---

## 8. Resumen Final

| **Parámetro**        | **Python (Imperativo)**          | **Haskell (Funcional)**         |
| -------------------- | -------------------------------- | ------------------------------- |
| **Claridad**         | Explícito pero verboso           | Conciso pero abstracto          |
| **Expresividad**     | Baja (instrucciones paso a paso) | Alta (declaración de criterios) |
| **Mutabilidad**      | Modifica datos in-place          | Genera nuevos datos             |
| **Manejo de Estado** | Complejo (variables mutables)    | Ninguno (funciones puras)       |
| **Mantenimiento**    | Propenso a errores               | Fácil de extender               |
| **Eficiencia**       | O(n²)                            | O(n log n)                      |

---

## 9. Conclusión General

- **Python** es ideal para problemas pequeños o cuando se necesita control detallado sobre el flujo de ejecución.
- **Haskell** es superior en escalabilidad, seguridad y mantenimiento para problemas complejos.

Este análisis demuestra que la elección del paradigma depende del problema, el equipo y los requisitos de rendimiento.
