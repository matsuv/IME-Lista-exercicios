--2.1 Faça um novo tipo chamado Mes , que possui como valores todos os meses do ano. Implemente
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
    deriving (Show, Enum, Eq)

--A função checaFim , que retorna o n´umero de dias que cada mˆes possui (considere fevereiro tendo 28 dias)
checaFim :: Mes -> Int
checaFim mes = dias !! fromEnum mes
  where
    dias = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

--A função prox , que recebe um mˆes atual e retorna o próximo mês.
prox :: Mes -> Int
prox mes
    |mes == Dezembro = dias !! fromEnum Janeiro        
    |otherwise  = dias !! (fromEnum mes + 1)
        where
            dias = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

--A função estacao , que retorna a esta¸c˜ao do ano de acordo com o mˆes e com o hemisf´erio. (Use apenas tipos criados pela palavra data aqui).

data Estacao = Primavera | Verao | Outono | Inverno
    deriving (Show, Eq)

data Hemisferio = Norte | Sul
    deriving (Show, Eq)

estacaoComBaseNoMes :: Hemisferio -> Mes -> Estacao
estacaoComBaseNoMes Norte mes
    | mes `elem` [Marco, Abril, Maio] = Primavera
    | mes `elem` [Junho, Julho, Agosto] = Verao
    | mes `elem` [Setembro, Outubro, Novembro] = Outono
    | otherwise = Inverno
estacaoComBaseNoMes Sul mes
    | mes `elem` [Marco, Abril, Maio] = Outono
    | mes `elem` [Junho, Julho, Agosto] = Inverno
    | mes `elem` [Setembro, Outubro, Novembro] = Primavera
    | otherwise = Verao