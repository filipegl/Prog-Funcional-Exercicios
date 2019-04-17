module BST (
        BinaryTree (..),
        sizeBST,
        isBST,
        search,
        maxim,
        minim,
        predecessor,
        successor,
        remove,
        preOrder,
        order,
        postOrder
)
where
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
