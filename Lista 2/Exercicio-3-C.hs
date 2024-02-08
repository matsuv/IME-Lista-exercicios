--Fa ̧ca um programa que calcule uma equa ̧c ̃ao do segundo grau, a partir dos dados digitados pelo usu ́ario
import System.IO

main :: IO ()
main = do
    putStrLn "Digite o coeficiente a:"
    valorA <- readLn :: IO Double

    putStrLn "Digite o coeficiente b:"
    valorB <- readLn :: IO Double

    putStrLn "Digite o coeficiente c:"
    valorC <- readLn :: IO Double

    let delta = valorB ^ 2 - 4 * valorA * valorC

    if delta < 0
        then putStrLn "A equação não possui raízes reais."
        else do
            let raiz1 = (-valorB + sqrt delta) / (2 * valorA)
                raiz2 = (-valorB - sqrt delta) / (2 * valorA)

            putStrLn $ "Raiz 1: " ++ show raiz1
            putStrLn $ "Raiz 2: " ++ show raiz2