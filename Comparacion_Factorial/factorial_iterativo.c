#include <stdio.h>

// Función para calcular el factorial de un número de manera iterativa
unsigned long long calcular_factorial_iterativo(int n) {
    unsigned long long resultado = 1;
    for (int i = 1; i <= n; i++) {
        resultado *= i;
    }
    return resultado;
}

int main() {
    int numero;
    printf("Introduce un número entero: ");
    scanf("%d", &numero);

    if (numero < 0) {
        printf("El factorial no está definido para números negativos.\n");
    } else {
        unsigned long long resultado = calcular_factorial_iterativo(numero);
        printf("El factorial de %d es %llu\n", numero, resultado);
    }

    return 0;
}

