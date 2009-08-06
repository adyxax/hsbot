module Main where
import System.Exit
import System.Plugins

ghcargs :: [String]
ghcargs = ["-XPatternGuards"] 

-- | Dynamic launching function
main :: IO ()
main = do
    putStrLn "hsbot starting..."
    m <- makeAll "Hsbot.hs" ghcargs
    (modul', imain) <- case m of
        MakeSuccess _ _ -> do
            ldstat <- load_ "Hsbot/Main.o" [".","Hsbot","Hsbot/Plugins"] "imain"
            case ldstat of
                LoadSuccess v m' -> return (v,m')
                LoadFailure e -> do
                    putStrLn "Couldn't load Hsbot.Main.imain:"
                    mapM_ putStrLn e
                    exitWith $ ExitFailure 127
        MakeFailure e -> do
            putStrLn "FATAL: Couldn't compile Hsbot.hs:"
            mapM_ putStrLn e
            exitWith $ ExitFailure 127
    putStrLn "Compiled & Loaded Hsbot.Main.imain..."
    imain modul' reboot

-- | Dynamic rebooting function
reboot :: Module -> a -> IO ()
reboot modul' state = do
    mkstat <- makeAll "Hsbot.hs" ghcargs
    case mkstat of
        MakeSuccess _ _ -> do
            unloadAll modul'
            ldstat <- load_ "Hsbot/Main.o" [".","Hsbot","Hsbot/Plugins"] "imain'"
            case ldstat of
                LoadSuccess modul'' imain' -> do
                    putStrLn "REBOOT: Successful recompilation & reloading, rebooting..."
                    imain' modul'' reboot state
                LoadFailure e -> fatality e
        MakeFailure e -> fatality e
    where
        fatality errs = do
            putStrLn $ "REBOOT: FATAL: Couldn't reboot thread, err:"
            mapM_ putStrLn errs

