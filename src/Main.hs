import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Text as T
import Control.Monad

import System.IO
import System.Directory (doesFileExist)
import System.Console.ANSI

import HProlog.Expr
import HProlog.Query
import HProlog.Parse

main = do
  hSetBuffering stdout NoBuffering
  loop []
  where loop :: [Rule] -> IO ()
        loop kb = do
          putStr ">> "
          queryStr <- getLine
          let queryStrWords = L.words queryStr
          if length queryStrWords == 0
          then loop kb
          else do
            case queryStrWords of
              "tell":queryStrTail' -> do
                case parseAssert $ L.unwords queryStrTail' of
                  Left err -> print err >> loop kb
                  Right rule -> loop (rule:kb)
                
              "ask":queryStrTail' -> do
                case parseQuery $ L.unwords queryStrTail' of
                  Left err -> print err >> loop kb
                  Right q -> do
                    let results = query kb q
                    if length results == 0
                    then do
                      putStrLn "No." >> loop kb
                      loop kb
                    else do
                      printResult q $ filter (\s -> length s > 0) results
                      loop kb

              "clear":[] -> do
                putStrLn "Knowledge base cleared."
                loop []

              "kb":[] -> do
                forM kb print
                loop kb

              "exit":[] ->
                return ()

              "quit":[] ->
                return ()

              "load":file:[] -> do
                fileExists <- doesFileExist file
                if fileExists
                then do
                  withFile file ReadMode $ \h -> do
                    filestr <- hGetContents h
                    case parseFile filestr of
                      Left err -> do
                        print err
                        loop kb
                    
                      Right asserts -> do
                        loop (kb ++ asserts)

                  else do
                    putStrLn "File doesn't exist!"
                    loop kb
                
              otherwise -> do
                putStrLn "Unknown command."
                loop kb

        printResult q []     = putStrLn "Yes."
        printResult q (s:ss) = do
          putStrLn (show q)
          putStrLn $ showSub s
          if length ss == 0
          then printResult q ss
          else do
            putStrLn "Next? (Y/N)"
            input <- getLine
            if map C.toLower input == "y"
            then printResult q ss
            else return ()

        showSub s =
          let showSubst (v,e) = (T.unpack v) ++ " => " ++ (show e) in
          L.intercalate "\n" $ map showSubst s

