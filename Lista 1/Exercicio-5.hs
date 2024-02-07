module Rec where
data Nat = Z | Suc Nat deriving Show
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n

-- input: natToInt (Suc (Suc (Suc (Suc Z))))
-- output: 4

somar :: Nat -> Nat -> Nat
somar x Z = x
somar x (Suc n) = Suc (somar x n)

-- input: somar (Suc (Suc Z)) (Suc (Suc (Suc Z)))
-- output: Suc (Suc (Suc (Suc (Suc Z))))

mult :: Nat -> Nat -> Nat
mult x Z = Z
mult x (Suc Z) = x
mult x (Suc n) = somar x (mult x n)

-- input: mult (Suc (Suc Z)) (Suc (Suc (Suc Z)))
-- output: Suc (Suc (Suc (Suc (Suc Z))))
fat :: Int -> Int
fat n
   | n <= 0 = 1
   | otherwise = n * fat (n - 1)

fatt :: Nat -> Nat
fatt Z = Suc Z
fatt (Suc n) = mult (Suc n) (fatt n)

fibb :: Nat -> Nat
fibb Z = Z
fibb (Suc Z) = Suc Z
fibb (Suc (Suc n)) = somar (fibb (Suc n)) (fibb n)