Comparación de Enfoques para el Cálculo del Factorial
Descripción
Este repositorio contiene un ejercicio práctico en el que se implementa el cálculo del factorial de un número entero usando dos enfoques en C: uno iterativo y otro recursivo. Además, se incluye un análisis comparativo del rendimiento y un estudio sobre cómo un enfoque funcional podría aportar mejoras en eficiencia y claridad.

Objetivos
Comparar la eficiencia (tiempo y memoria) de las implementaciones iterativa y recursiva.
Analizar las ventajas y desventajas de cada método.
Reflexionar sobre el impacto de adoptar un paradigma funcional.
Presentar un análisis argumentado basado en conceptos de programación funcional.
Estructura del Repositorio
bash
Copiar
Editar
/
├── factorial_iterativo.c        # Implementación iterativa en C
├── factorial_recursivo.c        # Implementación recursiva en C
├── informe_comparativo.md       # Comparación de desempeño (tiempo y memoria)
├── analisis_funcional.md        # Análisis del paradigma funcional
└── README.md                     # Enunciado y contexto general
Resumen del Contenido
Implementaciones en C:

factorial_iterativo.c: Calcula el factorial mediante un bucle for (de 1 a 
𝑛
n).
factorial_recursivo.c: Calcula el factorial usando recursión, siguiendo la definición matemática.
Informes y Análisis:

informe_comparativo.md: Analiza el rendimiento de ambas implementaciones (tiempo y uso de memoria).
analisis_funcional.md: Explora cómo el paradigma funcional (con funciones puras, recursión de cola, evaluación perezosa e inmutabilidad) puede mejorar este cálculo.
Compilación y Ejecución
Para compilar los programas en C, utiliza los siguientes comandos:

bash
Copiar
Editar
# Compilar la versión iterativa
gcc -o factorial_iterativo factorial_iterativo.c

# Compilar la versión recursiva
gcc -o factorial_recursivo factorial_recursivo.c
Para ejecutar los programas:

bash
Copiar
Editar
# Ejecutar la versión iterativa
./factorial_iterativo

# Ejecutar la versión recursiva
./factorial_recursivo
Conclusiones Principales
Eficiencia Temporal: La versión iterativa es consistentemente más rápida, especialmente para valores altos de 
𝑛
n.
Eficiencia Espacial: La iterativa utiliza memoria constante (O(1)), mientras que la recursiva usa memoria proporcional a 
𝑛
n (O(n)).
Paradigma Funcional: Ofrece ventajas como mayor claridad, optimizaciones automáticas y la posibilidad de manejar cálculos complejos mediante memoización.
