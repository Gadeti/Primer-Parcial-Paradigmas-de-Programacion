import Data.List (sortBy)

estudiantes :: [(String, Int)]
estudiantes = [
    ("Ana", 85),
    ("Luis", 90),
    ("Carlos", 85),
    ("Sofia", 92),
    ("Maria", 90)
    ]

-- Comparador 
comparador :: (String, Int) -> (String, Int) -> Ordering
comparador (n1, c1) (n2, c2)
  | c1 > c2 = LT   -- Orden descendente por calificación
  | c1 < c2 = GT
  | otherwise = compare n1 n2  -- Orden ascendente por nombre si hay empate

ordenarEstudiantes :: [(String, Int)] -> [(String, Int)]
ordenarEstudiantes = sortBy comparador

main :: IO ()
main = print $ ordenarEstudiantes estudiantes