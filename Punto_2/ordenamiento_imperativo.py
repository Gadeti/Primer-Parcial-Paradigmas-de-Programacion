def ordenamiento_bubble_sort(estudiantes):
    n = len(estudiantes)
    for i in range(n - 1):
        for j in range(n - 1 - i):
            # se recorre las listas primero por las calificaciones y si son iguales se compara con el nombre
            if (estudiantes[j][1] < estudiantes[j + 1][1]) or \
               (estudiantes[j][1] == estudiantes[j + 1][1] and estudiantes[j][0] > estudiantes[j + 1][0]):
                # Intercambio swap unicamente si es necesario
                estudiantes[j], estudiantes[j + 1] = estudiantes[j + 1], estudiantes[j]

# Lista de estudiantes (nombre, calificación)
estudiantes = [     
    ("Ana", 85),
    ("Luis", 90),
    ("Carlos", 85),
    ("Sofía", 92),
    ("María", 90)
]

# Se hace el llamado a la funcion para que se ordenen los estudiantes
ordenamiento_bubble_sort(estudiantes)

# Mostrar la lista con el ordenamiento
for estudiante in estudiantes:
    print(estudiante)
