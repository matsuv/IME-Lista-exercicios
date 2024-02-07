import Language.Haskell.TH (safe)
--3.1 Implemente uma fun¸c˜ao que filtre os n´umeros pares e outra que filtre os ´ımpares de uma lista recebida via parˆametro.
filtrarParesEImpares :: [Int] -> ([Int], [Int])
filtrarParesEImpares lista = (filtrarPares lista, filtrarImpares lista)
  where
    filtrarPares :: [Int] -> [Int]
    filtrarPares = filter (\x -> x `mod` 2 == 0) 

    filtrarImpares :: [Int] -> [Int]
    filtrarImpares = filter (\x -> x `mod` 2 /= 0) 

--3.2 Implemente o tipo Dinheiro que contenha os campos valor e correncia ( Real ou Dolar ), e uma fun¸c˜ao que converta todos os ”dinheiros”de uma lista para d´olar (e outra para real). Com isso, implemente fun¸c˜oes para:

--Filtrar todos os Dolares de uma lista de Dinheiro 
data Moeda = USD deriving (Show, Eq)

data Dinheiro = Dinheiro{moeda :: Moeda, valor :: Float} deriving (Show, Eq)

filtraDolar :: [Dinheiro] -> [Dinheiro]
filtraDolar listaDinheiro = [dinheiro | dinheiro <- listaDinheiro, moeda dinheiro == USD]

somaDolares :: [Dinheiro] -> Float
somaDolares dinheiroList = sum [valor | Dinheiro USD valor <- dinheiroList]