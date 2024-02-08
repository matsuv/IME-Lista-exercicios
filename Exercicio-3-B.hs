-- Fa¸ca um programa que mostre uma palavra em ordem reversa a partir de uma digitada pelo usu´ario
main :: IO ()
main = do
    putStrLn "Digite uma palavra:"
    input <- getLine
    let palavraReversa = reverse input
    putStrLn ("Palavra em ordem reversa: " ++ palavraReversa)