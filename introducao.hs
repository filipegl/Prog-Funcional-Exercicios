{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor p q = (p && (not q)) || (q && (not p))
impl a b = (not a) || b
equiv a b = ((not a) || b) && ((not b) || a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x y = x**y

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x = x * fatorial(x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
factors n = [x | x <- [1..n], (mod n x) == 0]
isPrime n = factors n == [1,n]

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
-- 1, 1, 2, 3, 5, 8, 13, 21...
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc a 0 = a
mdc a b = mdc b (mod a b)

{-
- Calcula um MMC de dois numeros.
- Resposta: A funcao filter ira retornar uma nova lista filtrada: A lista de todos os multiplos comuns. 
- Os elementos `n` dessa lista quando aplicados na funcao `divisivel x y n`, retornarao `True`.
- O mmc sera o primeiro elemento dessa lista. 
-}
mmc x y = head multiplos_comuns
  where multiplos_comuns = filter (divisivel x y) [(min x y)..x*y]
divisivel x y n = (mod n x == 0) && (mod n y == 0)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y
    |(mdc x y) == 1 = True
    |otherwise = False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [(a,b) | a <- filter isPrime [2..x-1], b <- filter isPrime [2..x-1], a+b == x]