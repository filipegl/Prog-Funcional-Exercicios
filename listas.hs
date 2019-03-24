{-
- Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!" 
-}
meuLast [] = error "Lista vazia!"
meuLast (x:xs)
    |xs == [] = x
    |otherwise = meuLast xs
{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
- Resposta: Se xs for [] entao x sera o penultimo e y sera o ultimo
-}
penultimo [] = error "Lista sem penultimo!"
penultimo (x:[]) = error "Lista sem penultimo!"
penultimo (x:y:xs)
    |xs == [] = x
    |otherwise = penultimo (y:xs)

{-
- Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}

elementAt 1 (x:xs) = x
elementAt n (x:xs) = elementAt (n-1) xs

{-
- Retorna o tamanho de uma lista.
- Resposta: Jeito rapido: length [1..10]
- Resposta do prof: 
    meuLength [] = 0
    meuLength xs = 1 + meuLength (tail xs)
-}
meuLength' n [] = n
meuLength' n (x:xs) = meuLength' (n+1) xs
meuLength xs = meuLength' 0 xs

{-
- Retorna o inverso de uma lista. 
- Resposta: Jeito rapido: reverse [1..10]
-}
meuReverso' ys [] = ys
meuReverso' ys (x:xs) = meuReverso' ([x] ++ ys) xs
meuReverso xs = meuReverso' [] xs

{-
- Diz se uma lista é palindrome. 
- Resposta: head e last retornam o primeiro e o ultimo elemento da lista respectivamente.
            init retorna todos os elementos exceto o ultimo.
            tail retorna todos os elementos exceto o primeiro.
-}
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs
    |(head xs == last xs) = isPalindrome (init (tail xs))
    |otherwise = False

{- Remove os elementos duplicados de uma listaEx: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
- Resposta: A funcao elem x xs eh True se x pertencer a xs
-}
compress' ys [] = ys
compress' ys (x:xs)
    |elem x xs = compress (remove x ys)
    |otherwise = compress' ys xs

compress xs = compress' xs xs

{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}

compact [] = []
compact [x] = [x]
compact (x:xs)
    |head xs == x = x:(compact xs)
    |not (elem x xs) = x:(compact xs)
    |otherwise = x:(compact ([x] ++ ys))
    where ys = remove x xs

{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
remove_n_times 0 x xs = xs
remove_n_times qtd x xs = remove_n_times (qtd-1) x (remove x xs)

encode [] = []
encode (x:xs) = (x, qtd):(encode removed)
    where qtd = length (filter (== x) (x:xs)) 
          removed = remove_n_times qtd x (x:xs)
{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
- Resposta do Prof: split xs i = [take i xs] ++ [drop i xs]
    take i xs: retorna uma sublista de xs da posicao 1 ate i 
    drop: retorna uma sublista de xs da posicao i+1 ate length xs
-}
split' xs 0 = xs
split' (a:b:[]) i = split' [a++[head b], tail b] (i-1)

split xs 0 = [[], xs]
split (x:xs) i = split' [[x], xs] (i-1)

{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
- Resposta: Outras solucoes:
    slice xs imin imax = drop (imin - 1) (take imax xs)
ou
    slice imin imax = (drop (imin - 1)).(take imax)
-}
slice xs imin imax = ((take (imax - firsts)).(drop firsts)) xs
    where firsts = imin - 1

{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt el pos xs = (take (pos-1) xs) ++ [el] ++ (drop (pos-1) xs)

{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos 
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}
minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

{-
- Calcula a soma de todos os elementos de uma lista usando foldr.
-}
mySum xs = foldr (+) 0 xs

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList [] = undefined
maxList xs = foldr (max) (last xs) xs

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}
buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++ [x]

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}

soma [] = 0
soma (x:xs) = x + soma xs
tamanho [] = 0
tamanho xs = 1 + tamanho (tail xs)

mean [] = 0
mean (x:[]) = x 
mean xs = fst soma_tamanho/ snd soma_tamanho
    where soma_tamanho = (soma xs, tamanho xs)

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}
myAppend xs ys = foldr (:) ys xs
