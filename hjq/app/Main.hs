module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hjq -- 自前のモジュール
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  -- ファイル、標準入力で処理を分ける
  case args of
    [query, file] -> do
      json <- B.readFile file
      printResult $ hjq json (T.pack query)
    [query] -> do
      json <- B.getContents
      printResult $ hjq json (T.pack query)
    _ -> do
      putStrLn $ "Invalid arguments error. : " ++ show args
      exitWith $ ExitFailure 1

-- 標準出力
printResult :: Either T.Text B.ByteString -> IO ()
printResult (Right s) = B.putStrLn s
printResult (Left s) = do
  T.putStrLn s
  exitWith $ ExitFailure 1