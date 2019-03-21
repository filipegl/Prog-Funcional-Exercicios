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
-}
isPalindrome (x:xs) = undefined

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
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
compact xs = undefined


{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
encode xs = undefined

{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}
split xs i = undefined

{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}
slice xs imin imax = undefined

{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt el pos xs = undefined

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
mySum xs = undefined

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList xs = undefined

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}
buildPalindrome xs = undefined

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}
mean xs = undefined

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}
myAppend xs ys = undefined
