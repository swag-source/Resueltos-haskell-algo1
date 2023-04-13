-- Ejercicio 2 -- 

{- 
Especificación:
    problema sumaDistintos (x : Z, y : Z, z: Z) : Z {
        pre(True)
        post(res = x + y + z <=> (x =/= y) ^ (y =/= z) ^ (x =/= z))
    }
 -}



esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m | mod n m == 0 = True
                 | otherwise = False



digitoUnidades :: Int -> Int
digitoUnidades n | n < 10 = n
                 | n `mod` 10 < 10 = n `mod` 10
                 | otherwise = (n `mod` 100) `mod` 10



digitoDecenas :: Int -> Int
digitoDecenas n | n < 10 = n
                | mod n 100 < 10 = 0 
                | otherwise =  div ((n `mod` 100) - ((n `mod` 100) `mod` 10)) 10

-- Ejercicio 3 --

hallarK :: Int -> Int -> Int
hallarK a b = -(a) `div` b 

estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b | (a * a + a * b * hallarK a b ) == 0 = True
                      | otherwise = False



-- Ejercicio 4 -- 

{--
Especificación:
    problema prodInterno (s: seq<ZxZ> , t: seq<ZxZ>) : l: seq<ZxZ> {
        pre(|s| ^ |t| =/= 0)
        post(res = <x_{i}, y_{j}>  <=> {xi, yj} pert. ZxZ )
    }
--} 

prodInterno :: [Int] -> [Int] -> [Int]
prodInterno [vx, wx] [vy, wy] = [vx * vy , wx * wy]


{--
Especificación:
    problema todoMenor (s: seq<RxR>, t: seq<RxR>) : B {
        pre(|s| ^ |t| =/= 0);
        post(res = true <=> (para todo i pert. Z) s[i] < t[i] );
    }
--}

todoMenor :: [Float] -> [Float] -> Bool
todoMenor [vx, wx] [vy, wy] | vx < vy && wx < wy = True
                            | otherwise = False

{--
Especificación:
    problema distanciaPuntos(s: seq<RxR>, t: seq<RxR>) : R {
        pre(|s| ^ |t| =/= 0);
        post(res = sqrt((s[i] - t[i])^2 + (s[j] - t[j] )^2)) con i =/= j)
    }
--}

distanciaPuntos :: [Float] -> [Float] -> Float
distanciaPuntos [vx, vy] [wx, wy] = sqrt((vx - wx)^2 + (vy - wy)^2)

{--
Especificación:
    problema sumaTerna (s: seq<ZxZxZ>) : Z {
        pre(|s| = 3)
        post(res = sum_{i=0}^{|s|- 1} s[i])
    }
--}

sumaTerna :: [Int] -> Int
sumaTerna [] = 0
sumaTerna (x:xs) = x + sum (xs)


{--
Especificación:
    problema sumarSoloMultiplos (s: seq<ZxZxZ> , n: Z) : Z {
        pre(|s| =/= 0)
        post(res = ((∀i en Z)( suma_{i=0}{|s|-1} if esMultiploDe (s[i] n)  then s[i] else 0 fi))
    }
--}

{- sumarSoloMultiplos :: [Int] -> Int -> Int
sumarSoloMultiplos [] n = 0
sumarSoloMultiplos (x:xs) n | x `mod` n = 0 == x + sum(xs)
                            | otherwise = sum(xs) -}

{-- Ejercicio 5 --}

todosMenores :: [Int] -> Bool
todosMenores [n1, n2, n3] | f n1 > g n1 && f n2 > g n2 && f n3 > g n3 = True
                          | otherwise = False

f :: Int -> Int 
f n | n <= 7 = n * n 
    | otherwise = 2*(n) - 1 

g :: Int -> Int
g n | esPar(n) = div n 2 
    | otherwise = 3*n + 1 

esPar :: Int -> Bool 
esPar n | mod n 2 == 0 = True
        | otherwise = not (esPar n)



{-- Ejercicio 6 --}

bisiesto :: Int -> Bool
bisiesto x | ( not (esMultiplo x 4) || (esMultiplo x 100) && not (esMultiplo x 400) ) = False
           | otherwise = True


{-- Funcion auxiliar  --}
esMultiplo :: Int -> Int -> Bool
esMultiplo n m  | mod n m == 0 = True
                | otherwise = False


{-- Ejercicio 7 --}

distanciaManhattan :: [Float] -> [Float] -> Float
distanciaManhattan [x1, x2, x3] [y1, y2, y3] = abs( x1 - y1 + x2 - y2 + x3 - y3)

                        

{-- Ejercicio 8 --}

comparar :: Int -> Int -> Int 
comparar a b | sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b) = 1 
             | sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b) = -1 
             | otherwise = 0

{-- Funcion auxiliar  --}
sumaUltimosDosDigitos :: Int ->  Int
sumaUltimosDosDigitos x = x `mod` 10 + div x 10 `mod` 10



{-- Ejercicio 9 --}

{-- especificación a) 
    problema f1 (n : Z) : Z {
        pre(True)
        post(res = if 0 then 1 else 0 fi))
    }
--}