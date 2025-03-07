#include <stdio.h>

// Función para calcular el factorial de un número de manera recursiva
unsigned long long calcular_factorial_recursivo(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * calcular_factorial_recursivo(n - 1);
    }
}

int main() {
    int numero;
    printf("Introduce un número entero: ");
    scanf("%d", &numero);

    if (numero < 0) {
        printf("El factorial no está definido para números negativos.\n");
    } else {
        unsigned long long resultado = calcular_factorial_recursivo(numero);
        printf("El factorial de %d es %llu\n", numero, resultado);
    }

    return 0;
}

