module Main where

    import System.IO 
    import TorresHanoi

    num :: IO Int
    num = do
        putStrLn "Introduce un numero"
        n <- getLine
        if (read n) > 0 then return (read n)
                        else do 
                            putStrLn "La altura de la torre debe ser positiva!"
                            num

    menu :: IO Char
    menu = do
        putStrLn "\tp.- Asistencia generando ayuda por pantalla"
        putStrLn "\tf.- Asistencia generando un fichero txt"
        putStrLn "\tt.- Terminar la asistencia"
        putStrLn "--------------------------------- Teclee opcion valida: "
        opcion <- getChar
        if elem opcion "pft" then return opcion
                             else menu

    main = do
        n_pisos <- num
        putStrLn ("La altura de la Torre de Hanoi ha sido fijada en: " ++ show n_pisos ++ " pisos")
        opc <- menu
        putStrLn ("\nOpcion: "  ++ [opc])
        case opc of
            'p' -> do
                pasosTorresHanoi n_pisos
                main
            'f' -> do
                putStrLn "Introduce el nombre del fichero resultado: "
                nombre <- getLine
                pasosTorresHanoi_txt n_pisos nombre
                main
            't' -> putStrLn "Fin del programa"
