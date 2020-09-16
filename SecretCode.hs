module SecretCode where

import Data.Char
import Data.Maybe
import Test.HUnit

code :: [(Char, Char)]
code = zip ['a' .. 'z'] cypher ++ zip ['A' .. 'Z'] (map toUpper cypher)
  where
    cypher :: String
    cypher = "thequickbrownfxjmpsvlazydg"

encodeChar :: Char -> Char
encodeChar c = encodeHelper (lookup c code) c
  where
    encodeHelper (Just x) _ = x
    encodeHelper _ c = c

testEncodeChar =
  runTestTT $
    TestList
      [ encodeChar 'a' ~?= 't',
        encodeChar '.' ~?= '.'
      ]

encodeLine :: String -> String
encodeLine [] = []
encodeLine (h : xs) = (encodeChar h) : (encodeLine xs)

testEncodeLine = runTestTT $ TestList [encodeLine "abc defgh" ~?= "the quick"]

encodeContent :: String -> String
encodeContent str = (unlines . reverse . encodeLines . lines) str
  where
    encodeLines [] = []
    encodeLines (l1 : rest) = (encodeLine l1) : (encodeLines rest)

testEncodeContent =
  runTestTT $
    encodeContent "abc\n defgh\n" ~?= " quick\nthe\n"

encodeFile :: FilePath -> IO ()
encodeFile f = do
  let outFile = f ++ ".code"
  str <- readFile f
  writeFile outFile (encodeContent str)

main :: IO ()
main = do
  putStrLn "What file shall I encode?"
  fn <- getLine
  encodeFile fn

  putStrLn "All done!"
