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
                x -> if (null (tail x)) then head x else meuLast (tail x)
penultimo = \x -> case x of
                [] ->  error "Lista sem penultimo!"
                x -> if (null (tail x)) then  error "Lista sem penultimo!" else if (length (tail (tail x)) == 0) then head x else penultimo (tail x)
elementAt = \i xs -> if (i == 1) then head xs else elementAt (i-1) (tail xs) 
meuLength = \xs -> if (null xs) then 0 else 1 + meuLength (tail xs)
meuReverso = \xs -> if (null xs) then xs else meuReverso (tail xs) ++ [head xs] 
isPalindrome = \(x:xs) -> if (null (x:xs) || null xs) then True else if ([x] ++ [1] == [last xs] ++ [1]) then isPalindrome (init xs) else False
compress' = \ys xs -> if (null xs) then ys else if (elem (head xs) (tail xs)) then compress (remove (head xs) ys) else compress' ys (tail xs)
compress = \xs -> compress' xs xs
compact = \xs -> if (null xs || xs == [head xs]) then xs else if (head (tail xs) == head xs) then (head xs):(compact (tail xs)) else if (not (elem (head xs) (tail xs))) then (head xs):(compact (tail xs)) else (head xs):(compact ([(head xs)] ++ remove (head xs) (tail xs)))
remove = \e es -> if([e] ++ [1] == [head es] ++ [1]) then tail es else (head es):(remove e (tail es))
remove_n_times = \qtd x xs -> if (qtd == 0) then xs else remove_n_times (qtd-1) x (remove x xs)
encode = \xs -> if (null xs) then [] else ((head xs), length (filter (== (head xs)) xs)):(encode (remove_n_times (length (filter (== (head xs)) xs)) (head xs) xs))
split = \xs i -> [take i xs] ++ [drop i xs]
slice = \xs imin imax -> drop (imin - 1) (take imax xs)
insertAt = \el pos xs -> (take (pos-1) xs) ++ [el] ++ drop (pos-1) xs
minList = \xs -> if (null (tail xs)) then head xs else if ((head xs) < (minList (tail xs))) then (head xs) else minList (tail xs)
sort = \xs -> if (null xs) then [] else (minList xs):(sort (remove (minList xs) xs))
--mySum = \xs -> foldr (+) 0 xs     Erro de ambiguidade
maxList = undefined
buildPalindrome = \xs -> if (null xs) then xs else [head xs] ++ buildPalindrome (tail xs) ++ [head xs] 
soma = \xs -> if (null xs) then 0 else (head xs) + soma (tail xs)
tamanho = \xs -> if (null xs) then 0 else 1 + tamanho (tail xs)
mean = \xs -> if (null xs) then 0 else if (null (tail xs)) then head xs else fst (soma xs, tamanho xs)/snd (soma xs, tamanho xs)
myAppend xs ys = undefined
