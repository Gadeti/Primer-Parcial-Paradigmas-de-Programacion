#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Estructura del estudiante con memoria optimizada
typedef struct {
    char *nombre;
    char *apellido;
    unsigned short edad;
    unsigned int id;
    float *calificaciones;
} Estudiante;

// Variables globales
Estudiante *estudiantes = NULL;
size_t num_estudiantes = 0;
size_t memoria_usada = 0;

// Función para agregar un estudiante
void agregarEstudiante(char *nombre, char *apellido, unsigned short edad, unsigned int id, float *calificaciones) {
    estudiantes = realloc(estudiantes, (num_estudiantes + 1) * sizeof(Estudiante));
    if (!estudiantes) {
        printf("Error al asignar memoria.\n");
        return;
    }

    estudiantes[num_estudiantes].nombre = strdup(nombre);
    estudiantes[num_estudiantes].apellido = strdup(apellido);
    estudiantes[num_estudiantes].edad = edad;
    estudiantes[num_estudiantes].id = id;
    estudiantes[num_estudiantes].calificaciones = malloc(5 * sizeof(float)); // Suponiendo 5 calificaciones fijas
    memcpy(estudiantes[num_estudiantes].calificaciones, calificaciones, 5 * sizeof(float));

    size_t memoria_estudiante = sizeof(Estudiante) + strlen(nombre) + strlen(apellido) + 2 + 5 * sizeof(float);
    memoria_usada += memoria_estudiante;
    num_estudiantes++;

    printf("Estudiante \"%s %s\" agregado correctamente. Memoria utilizada: %lu bytes.\n", nombre, apellido, memoria_estudiante);
}

// Función para eliminar un estudiante
void eliminarEstudiante(unsigned int id) {
    for (size_t i = 0; i < num_estudiantes; i++) {
        if (estudiantes[i].id == id) {
            size_t memoria_liberada = sizeof(Estudiante) + strlen(estudiantes[i].nombre) + strlen(estudiantes[i].apellido) + 2 + 5 * sizeof(float);
            memoria_usada -= memoria_liberada;
            free(estudiantes[i].nombre);
            free(estudiantes[i].apellido);
            free(estudiantes[i].calificaciones);

            estudiantes[i] = estudiantes[num_estudiantes - 1];
            num_estudiantes--;
            if (num_estudiantes > 0) {
                estudiantes = realloc(estudiantes, num_estudiantes * sizeof(Estudiante));
            } else {
                free(estudiantes);
                estudiantes = NULL;
            }
            
            printf("Estudiante con ID %u eliminado correctamente. Memoria liberada: %lu bytes.\n", id, memoria_liberada);
            return;
        }
    }
    printf("Estudiante con ID %u no encontrado.\n", id);
}

// Función para mostrar la información de los estudiantes
void mostrarEstudiantes() {
    printf("\nLista de estudiantes:\n");
    for (size_t i = 0; i < num_estudiantes; i++) {
        printf("ID: %u, Nombre: %s %s, Edad: %u\n", estudiantes[i].id, estudiantes[i].nombre, estudiantes[i].apellido, estudiantes[i].edad);
        printf("Calificaciones: ");
        for (size_t j = 0; j < 5; j++) {
            printf("%.2f ", estudiantes[i].calificaciones[j]);
        }
        printf("\n");
    }
}

// Función para mostrar el uso de memoria
void mostrarUsoMemoria() {
    printf("Memoria utilizada: %lu bytes\n", memoria_usada);
}

int main() {
    int opcion;
    do {
        printf("\n1. Agregar estudiante\n");
        printf("2. Eliminar estudiante\n");
        printf("3. Mostrar estudiantes\n");
        printf("4. Mostrar uso de memoria\n");
        printf("5. Salir\n");
        printf("Seleccione una opción: ");
        scanf("%d", &opcion);
        getchar(); // Consumir nueva línea

        if (opcion == 1) {
            char nombre[50], apellido[50];
            unsigned short edad;
            unsigned int id;
            
            printf("Ingrese nombre: ");
            fgets(nombre, sizeof(nombre), stdin);
            nombre[strcspn(nombre, "\n")] = 0;

            printf("Ingrese apellido: ");
            fgets(apellido, sizeof(apellido), stdin);
            apellido[strcspn(apellido, "\n")] = 0;

            printf("Ingrese edad: ");
            scanf("%hu", &edad);
            printf("Ingrese ID: ");
            scanf("%u", &id);
            
            float calificaciones[5];
            printf("Ingrese 5 calificaciones: ");
            for (size_t i = 0; i < 5; i++) {
                scanf("%f", &calificaciones[i]);
            }

            agregarEstudiante(nombre, apellido, edad, id, calificaciones);
        } else if (opcion == 2) {
            unsigned int id;
            printf("Ingrese ID del estudiante a eliminar: ");
            scanf("%u", &id);
            eliminarEstudiante(id);
        } else if (opcion == 3) {
            mostrarEstudiantes();
        } else if (opcion == 4) {
            mostrarUsoMemoria();
        }
    } while (opcion != 5);
    
    free(estudiantes);
    return 0;
}
