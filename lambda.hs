--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow = \x y -> if (y == 0) then 1 else if (y > 0) then x*(pow x (y-1)) else 1/(pow x (-y))
fatorial = \x -> if (x == 0) then 1 else x*fatorial(x-1)
isPrime = \x -> (\n -> [y | y <- [1..n], (mod n y) == 0]) x == [1,x]
fib = \x -> case x of
                0 -> 0
                1 -> 1
                x -> fib (x-1) + fib (x-2)

mdc = \x y -> if (x == 0) then y else if (y == 0) then x else mdc y (mod x y)
mmc = \x y -> head (filter ((\x y n -> (mod n x == 0) && (mod n y == 0)) x y) [(min x y)..x*y])
coprimo = \x y -> if (mdc x y) == 1 then True else False 
goldbach = \x -> [(a,b) | a <- filter isPrime [2..x-1], b <- filter isPrime [2..x-1], a+b == x]

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast = \x -> case x of
                [] -> error "Lisa vazia!"
                x -> if (length (tail x) == 0) then head x else meuLast (tail x)
penultimo xs = undefined
elementAt i xs = undefined
meuLength xs = undefined
meuReverso xs = undefined
isPalindrome xs = undefined
compress xs = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined
