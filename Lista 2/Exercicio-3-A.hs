main :: IO ()
main = do
    putStrLn "Digite um número:"
    input <- getLine
    let numero = read input :: Int

    if even numero
        then putStrLn "O número é par."
        else putStrLn "O número é ímpar."