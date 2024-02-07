import Text.Read.Lex (Lexeme(Char))
--1.1 Construa o list comprehension que gere:

--Método Auxiliar
foraLista :: (Eq a) => a -> [a] -> Bool
foraLista _ [] = True
foraLista elemento (x : xs)
  | elemento == x = False
  | otherwise = foraLista elemento xs

-- (a) [(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
cincoEmCinco :: [Int] -> [Int]
cincoEmCinco xs = [x | x <- xs, x `mod` 5 == 0,x <= 50]

--(b) Uma lista de ’a’ a ’z’ sem as vogais.
semVogal :: [Char] -> [Char]
semVogal xs = [x | x <- xs,foraLista x ['a', 'e', 'i', 'o', 'u']]


--(c) Uma lista de 0 a 50 sem os n´umeros 2, 7, 13, 35 e 42
removerElementosEspecificos :: [Int] -> [Int]
removerElementosEspecificos xs = [x | x <- xs, foraLista x [2, 7, 13, 35, 42]]

-- (d) Uma lista com todas as coordenadas dde um tabuleiro de damas 8x8: 
montarDamas :: [Char] -> [Int] -> [(Char, Int)]
montarDamas s' s'' = [(x, y) | x <- s', y <- s'']

-- (e) Crie uma função que verifique se o tamanho de uma String ´e par ou n˜ao. Use Bool como retorno.
verificarTamanhoPalavraEhPar :: String -> Bool
verificarTamanhoPalavraEhPar palavra = even (length palavra)


--1.2 Implemente as seguintes funções:
 
--(b) Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os elementos em ordem reversa
reverterString :: [String] -> [String]
reverterString [] = []
reverterString elemento = reverse elemento

--(c) Escreva a fun¸c˜ao head como composi¸c˜ao de duas outras
funcaoHead :: [a] -> a
funcaoHead = last . take 1




