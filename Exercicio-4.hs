data Lista a = Nulo | a :>: (Lista a) deriving Show

--Implemente a fun¸c˜ao removerElemento que recebe um elemento ”a”qualquer, uma Lista a e retorne uma Lista a com o elemento removido. removerElemento :: a -> Lista a -> Lista a
removerElemento :: (Eq a) => a -> Lista a -> Lista a
removerElemento _ Nulo = Nulo
removerElemento elemento (x :>: restante)
    | elemento == x = restante
    | otherwise = x :>: removerElemento elemento restante


data Paridade = Par | Impar deriving Show

class ParImpar a where
    decide :: a -> Paridade

instance ParImpar Int where
    decide n
        | even n    = Par
        | otherwise = Impar

instance ParImpar [a] where
    decide lista
        | even (length lista) = Par
        | otherwise           = Impar

instance ParImpar Bool where
    decide True  = Impar
    decide False = Par