data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

--Faca uma instância de Functor para o tipo Coisa. A função g deve ”ir para dentro” em todas as coordenadas de Coisa . No caso de ZeroCoisa , o fmap deve retornar ZeroCoisa 

instance Functor Coisa where
    fmap _ ZeroCoisa         = ZeroCoisa
    fmap g (UmaCoisa a)      = UmaCoisa (g a)
    fmap g (DuasCoisas a b)  = DuasCoisas (g a) (g b)

instance Applicative Coisa where
    pure = UmaCoisa
    ZeroCoisa <*> _           = ZeroCoisa
    _ <*> ZeroCoisa           = ZeroCoisa
    UmaCoisa g <*> c          = g <$> c
    DuasCoisas g h <*> c      = DuasCoisas (g <$> c) (h <$> c)


mult234 :: Double -> Coisa Double
mult234 x = DuasCoisas (*2) (*3) <*> UmaCoisa x