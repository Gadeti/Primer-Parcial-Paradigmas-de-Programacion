Análisis: Mejoras mediante el Paradigma Funcional
Introducción
Este análisis examina cómo un enfoque funcional puede mejorar el cálculo del factorial, comparado con las implementaciones en C. La programación funcional se basa en funciones puras y evita estados mutables, lo cual aporta claridad y eficiencia.

Características Clave del Paradigma Funcional
1. Funciones Puras
Siempre producen el mismo resultado para un mismo input.
No tienen efectos secundarios.
Ejemplo en Haskell:
haskell
Copiar
Editar
factorial :: Integer -> Integer
factorial n = product [1..n]
2. Recursión de Cola (Tail Recursion)
La llamada recursiva es la última operación, permitiendo al compilador optimizar el uso de la pila.
Ejemplo en Haskell:
haskell
Copiar
Editar
factorialTR :: Integer -> Integer
factorialTR n = aux n 1
  where
    aux 0 acc = acc
    aux n acc = aux (n-1) (n * acc)
3. Evaluación Perezosa (Lazy Evaluation)
Se evalúan las expresiones solo cuando se necesitan.
Ejemplo Conceptual:
haskell
Copiar
Editar
factorials = 1 : zipWith (*) [1..] factorials
factorial n = factorials !! n
4. Inmutabilidad
Los datos no cambian una vez creados, facilitando la memoización y reduciendo errores.
Ejemplo de Memoización en Haskell:
haskell
Copiar
Editar
factorialMemo :: Integer -> Integer
factorialMemo = (map fac [0..] !!)
  where 
    fac 0 = 1
    fac n = n * factorialMemo (n-1)
Comparación con Enfoques Imperativos
Aspecto	Iterativo (Imperativo)	Recursivo (Imperativo)	Funcional
Claridad conceptual	Media	Alta	Muy alta
Eficiencia temporal	Alta	Baja	Alta (con optimizaciones)
Eficiencia espacial	Alta (O(1))	Baja (O(n))	Alta con TR (O(1))
Escalabilidad	Limitada	Muy limitada	Excelente
Mantenibilidad	Media	Media	Alta
Paralelización	Difícil	Muy difícil	Natural
Implementación en Haskell (Ejemplo Completo)
haskell
Copiar
Editar
module Factorial where

-- Versión simple usando product
factorial1 :: Integer -> Integer
factorial1 n = product [1..n]

main :: IO ()
main = do
  putStrLn "Introduce un número para calcular su factorial:"
  input <- getLine
  let n = read input :: Integer
  putStrLn $ "Resultado usando product: " ++ show (factorial1 n)
Conclusiones
El enfoque funcional ofrece:

Mayor expresividad y claridad.
Optimización automática (por ejemplo, recursión de cola y evaluación perezosa).
Mejor mantenibilidad y facilidad para paralelizar el código.

