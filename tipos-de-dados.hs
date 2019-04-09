--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving (Eq,Show)

firstTwo (Quadruple a b c d) = (a, b)
secondTwo (Quadruple a b c d) = (c, d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq,Show)

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b)= Just b
tuple2 (Tuple3 a b c)= Just b
tuple2 (Tuple4 a b c d)= Just b 
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c 
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
--TODO
--isBST (Node a left right)
--	| a < left = (isBST left) && (isBST right)
--	| otherwise = False

--insere uma nova chave na BST retornando a BST modificada
insert = undefined

--retorna o Node da BST contendo o dado procurado ou entao NIL
search element (Node a left right)
    | (element == a) = (Node a left right)
    | ((left == NIL) && (right == NIL)) = NIL
    | (element > a) = search element right
    | (element < a) = search element left

--retorna o elmento maximo da BST
maxim (Node a left right)
    | (right /= NIL) = maxim right
    | otherwise = a

--retorna o elemento minimo da BST
minim (Node a left right)
    | (left /= NIL) = minim left
    | otherwise = a

-- algoritimo (java) em: https://www.techiedelight.com/find-inorder-predecessor-given-key-bst/
getParentPredecessor element prec (Node a left right)
    | (root == NIL) = prec
    | (element == a) = if (left /= NIL) then maxim left else prec
    | (element < a) = getParentPredecessor element prec left
    | otherwise = getParentPredecessor element a right
    where root = (Node a left right) 
--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
-- chamada: predecessor elemento bst
predecessor element (Node a left right)
    | (node == NIL) = error "Not Found"
    | (element == (minim node)) = error "Has not predecessor"
    | otherwise = getParentPredecessor element a (Node a left right)
    where node = search element (Node a left right)


--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
-- obs: o codigo `(\(Just x) -> x) rightElem` e um converor do tipo Maybe.
successor element (Node a left right)
    | (node == NIL) = error "Not Found"
    | (rightElem == Nothing) = error "This node is a leaf"
    | otherwise = (\(Just x) -> x) rightElem
    where node = search element (Node a left right)
          rightElem = getRight node

getElement NIL = Nothing
getElement (Node a left right) = (Just a)

getLeft (Node a left right) = getElement left

getRight (Node a left right) = getElement right

--`isParent element bst` retorna True se a bst for nó pai do elemento.
isParent element (Node a left right)
    | ((getElement left) == element) = True
    | ((getElement right) == element) = True
    | otherwise = False

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined

-- arvore representada nesta imagem: 
-- https://cdncontribute.geeksforgeeks.org/wp-content/uploads/BST.png
vinteecinco = (Node 25 NIL NIL)
dezenove = (Node 19 NIL NIL)
vinteeum = (Node 21 dezenove vinteecinco)
doze = (Node 12 nove vinteeum)
nove = (Node 9 NIL NIL)
tres = (Node 3 NIL NIL)
um = (Node 1 NIL NIL)
dois = (Node 2 um tres)
cinco = (Node 5 dois doze) -- raiz
-- exemplo de operacao: predecessor 19 cinco = 12