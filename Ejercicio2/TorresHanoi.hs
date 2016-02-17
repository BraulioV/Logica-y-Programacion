module TorresHanoi (pasosTorresHanoi, pasosTorresHanoi_txt) where

    import System.IO
    import Data.Char

    pasosTorresHanoiAux :: Int -> Char -> Char -> [Char] -> String
    pasosTorresHanoiAux a s t r = "mover disco " ++ [(r !! (a-1))] ++ " desde " ++ [s] ++ " hasta " ++ [t]

    pasosTorresHanoi' :: Int -> Char -> Char -> Char -> [Char] -> [String]
    pasosTorresHanoi' 0 _ _ _ _ = []
    pasosTorresHanoi' a s h t r = pasosTorresHanoi' (a-1) s t h r ++ [pasosTorresHanoiAux a s t r] ++ pasosTorresHanoi' (a-1) h s t r

    pasosTorresHanoi :: Int -> IO()
    pasosTorresHanoi a = putStrLn (unlines (pasosTorresHanoi' a 'A' 'B' 'C' discos))
        where
            discos = reverse' (map (intToDigit) [1..a])

    reverse' :: [a] -> [a]
    reverse' xs = reverseAux xs []
      where
        reverseAux :: [a] -> [a] -> [a]
        reverseAux [] lst     = lst
        reverseAux (x:xs) lst = reverseAux xs (x:lst)

    pasosTorresHanoi_txt :: Int -> String -> IO()
    pasosTorresHanoi_txt x nombre = writeFile nombre (unlines (pasosTorresHanoi' x 'A' 'B' 'C' discos))
        where
            discos = reverse' (map (intToDigit) [1..x])
