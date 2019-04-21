module MultisetList (
  insert,
  search,
  remove,
  union,
  intersection,
  minus,
  inclusion,
  MultisetList.sum,
  size)

 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorrências do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
--  O `qualified` exige que eu me refira as funcoes de List como List.x
import qualified Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert' elem (x:xs)
  | ((fst x) == elem) = (elem, (snd x) + 1):xs
  | otherwise = x:(insert' elem xs)

insert elem bag
  | (search elem bag == 0) = bag ++ [(elem, 1)]
  | otherwise = insert' elem bag

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem (x:xs)
  | amount <= 1 = List.delete pair (x:xs)
  | (fst x) == elem = (elem, (snd x) - 1):xs
  | otherwise = x:(remove elem xs)
  where amount = search elem (x:xs)
        pair = (elem, amount)

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem bag = if (null elem_found) then 0 else snd (head elem_found)
  where elem_found = [pair | pair <- bag, (fst pair) == elem]

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union bag1 [] = bag1
union [] bag2 = bag2
union bag1 bag2
  | (amount2 == 0) = h1:(union t1 bag2)
  | ((snd h1) >= amount2) = h1:(union t1 new_bag2)
  | otherwise = pair2:(union t1 new_bag2)
  where h1 = head bag1
        t1 = tail bag1
        amount2 = search (fst h1) bag2
        pair2 = ((fst h1), amount2)
        new_bag2 = List.delete pair2 bag2

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection [] bag2 = []
intersection bag1 [] = []
intersection bag1 bag2
  | (amount2 == 0) = intersection t1 bag2
  | ((snd h1) >= amount2) = pair2:(intersection t1 bag2)
  | (null t1) = [h1]
  | otherwise = h1:(intersection t1 bag2)
  where h1 = head bag1
        t1 = tail bag1
        amount2 = search (fst h1) bag2
        pair2 = ((fst h1), amount2)

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 [] = bag1
minus [] bag2 = []
minus bag1 bag2
  | (amount2 == 0) = h1:(minus t1 bag2)
  | ((snd h1) >= amount2) = pair2:(minus t1 bag2)
  | otherwise = minus t1 bag2
  where h1 = head bag1
        t1 = tail bag1
        amount2 = search (fst h1) bag2
        pair2 = ((fst h1), (snd h1) - amount2)

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion [] bag2 = True
inclusion bag1 [] = False
inclusion bag1 bag2
  | (amount2 == 0) = False
  | ((snd h1) > amount2) = False
  | otherwise = inclusion t1 bag2
  where h1 = head bag1
        t1 = tail bag1
        amount2 = search (fst h1) bag2

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum bag1 [] = bag1
sum [] bag2 = bag2
sum bag1 bag2
  | (amount2 == 0) = h1:(MultisetList.sum t1 bag2)
  | otherwise = pair2:(MultisetList.sum t1 new_bag2)
  where h1 = head bag1
        t1 = tail bag1
        amount2 = search (fst h1) bag2
        pair2 = ((fst h1), amount2 + snd h1)
        to_remove = ((fst h1), amount2)
        new_bag2 = List.delete to_remove bag2

{-
 - Retorna a quantidade total de elementos no Bag
-}
size [] = 0
size (x:xs) = snd x + size xs