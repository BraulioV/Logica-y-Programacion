module TorresHanoi (module Congruencia) where

    pasosTorresHanoi :: Int -> IO()
    pasosTorresHanoi 1 = putStrLn "Mueva el disco superior de Izq a Der"
    pasosTorresHanoi x
        | odd x     = putStrLn "Mueva el disco superior de Izq a Der"
        | otherwise = putStrLn "Mueva el disco superior de Izq a Cen"

    pasosTorresHanoi :: (Int, Int, Int) -> IO()
    pasosTorresHanoi (x,0,0)
        | odd x     = putStrLn "Mueva el disco superior de Izq a Der"
        | otherwise = putStrLn "Mueva el disco superior de Izq a Cen"
    pasosTorresHanoi (x,y,z)n