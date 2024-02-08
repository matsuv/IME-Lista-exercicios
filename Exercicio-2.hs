data Caixa a = Um a | Dois a a | Tres a a a deriving Show

instance Functor Caixa where
    fmap f (Um a) = Um (f a)
    fmap f (Dois a b) = Dois (f a) (f b)
    fmap f (Tres a b c) = Tres (f a) (f b) (f c)

instance Applicative Caixa where
    pure = Um
    Um f <*> caixa = f <$> caixa
    --Dois f g <*> caixa = Dois (f <$> caixa) (g <$> caixa)
    --Tres f g h <*> caixa = Tres (f <$> caixa) (g <$> caixa) (h <$> caixa)

instance Monad Caixa where
    return = Um
    Um a >>= f = f a
    Dois a b >>= f = case f a of
                        Um x -> Dois x b
                        Dois x y -> Dois x y 
                        Tres x y z -> Dois x y
    Tres a b c >>= f = case f a of
                        Um x -> Tres x b c
                        Dois x y -> Tres x y c
                        Tres x y z -> Tres x y z