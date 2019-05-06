{-
Neste exercicio voce vai implementar um menu que permite o usuario escolher
a sua versao de multiconjunto implementada (multiset) com lista ou map.

Primeiramente o menu deve oferecer escolha de que implementação testar. depois ele 
deve oferecer opcoes de invocar cada funcao presente no multiset e executa-la 
quando escolhida. 

Duas funcionalidades devem sempre estar presentes para cada estrutura: a impressão da estrutura em si
para que ela seja visualizada, e uma funcao que ordena os elementos da estrutura por chave/valor ou pela 
quantidade de ocorrencias. No caso da função de ordenação ser invocada, ela deve imprimir a estrutura
apos a ordenação.
-}
import MultisetList
import MultisetMap

menu :: IO String
menu = do
    let menutxt = unlines ["",
                        "Qual implementacao deseja testar?",
                        "Multiset.list ....... 1",
                        "Multiset.map ........ 2",
                        "",
                        "Sair ................ 0"]

    putStrLn $ menutxt ++ "Opcao: "
    c <- getLine
    return c
            
funcs = do
    let f = unlines [
            "",
            "insert ........... 1",
            "search ........... 2",
            "remove ........... 3",
            "union ............ 4",
            "intersection ..... 5",
            "minus ............ 6",
            "inclusion ........ 7",
            "sum .............. 8",
            "size ............. 9",
            "imprimir ......... 10",
            "ordenar .......... 11",
            "",
            "sair ............. 0"]
    putStrLn f
    o <- readLn :: IO Int
    return o

list_loop list option = do
  case option of
    0 -> return ()
        
    1 -> do 
        putStrLn "Elemento: "
        e <- getLine
        opt <- funcs
        list_loop (MultisetList.insert e list) opt
                
    10 -> do
        print list
        putStrLn ""
        opt <- funcs
        list_loop list opt

map_loop map option = return ()

main = do
    set <- menu
    opt <- funcs
   
    if ((read set) == 1) then list_loop [] opt else return ()
    
