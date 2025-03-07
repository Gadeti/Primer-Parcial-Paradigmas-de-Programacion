# Gestión Dinámica y Optimización de Memoria para Registros de Estudiantes

Un sistema diseñado para gestionar los datos de estudiantes en una universidad. Cada alumno cuenta con un registro que contiene su **nombre**, **apellido**, **edad**, **número de identificación** y una lista de **calificaciones asociadas a sus asignaturas**. Debido a las restricciones de memoria en el sistema, es crucial implementar estrategias que optimicen la manera de almacenar estos registros.

## Técnicas Utilizadas
Las técnicas aplicadas incluyeron el desarrollo de un programa en **C** para la gestión dinámica de estudiantes:

- **Asignación y desasignación de memoria en el heap** mediante `malloc`, `realloc` y `free`, evitando desperdicio y fragmentación.
- Uso de `strdup` para asignar memoria solo cuando sea necesaria.
- Creación de funciones para agregar, eliminar y mostrar estudiantes, junto con una función para rastrear el uso de memoria.

## Funciones del Programa
- **agregarEstudiante**: Agrega un estudiante al arreglo dinámico, asignando memoria y copiando datos.
- **eliminarEstudiante**: Busca un estudiante por ID, libera su memoria y reorganiza el arreglo.
- **mostrarEstudiantes**: Muestra los datos de los estudiantes.
- **mostrarUsoMemoria**: Informa cuánta memoria está siendo utilizada.
- **main**: Controla el flujo del programa a través de un menú.

## Funciones de Manipulación de Memoria
- `malloc`: Reserva un bloque de memoria en el heap.
- `realloc`: Ajusta el tamaño de un bloque de memoria existente.
- `free`: Libera memoria reservada para evitar fugas.
- `strdup`: Duplica cadenas de texto reservando memoria.
- `memcpy`: Copia datos entre bloques de memoria.

## Comparación de Desempeño
| Métrica                | Sin optimización | Con optimización |
|------------------------|------------------|------------------|
| Memoria utilizada total | 556,828 bytes    | 476,573 bytes    |
| Fragmentación detectada | Alta             | Baja o nula      |
| Tiempo de ejecución     | 20.2 ms          | 32.2 ms          |

### Análisis de la Tabla
- **Memoria utilizada**: Reducción de aproximadamente **80 KB**, lo que indica una gestión eficiente.
- **Fragmentación detectada**: Reducción significativa, mejorando estabilidad y eficiencia.
- **Tiempo de ejecución**: Incremento de **12 ms**, resultado de operaciones adicionales de optimización.

## Conclusión
La optimización logró un uso más eficiente de la memoria y redujo la fragmentación, 
siendo beneficioso para aplicaciones sensibles a la estabilidad y gran cantidad de 
datos. Sin embargo, se observó un incremento en el tiempo de ejecución, lo cual puede
 requerir un balance según los requisitos del sistema.
