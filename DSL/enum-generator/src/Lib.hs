module Lib
    ( someFunc
    ) where

import EnumDSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    putStrLn "=== Enum DSL Generator ==="
    putStrLn ""
    
    -- 예제 enum 정의
    let exampleInput = T.unlines
            [ T.pack "enum Color : uint8 {"
            , T.pack "    RED,"
            , T.pack "    GREEN,"
            , T.pack "    BLUE,"
            , T.pack "}"
            , T.pack ""
            , T.pack "enum WeekOfDay : uint8 {"
            , T.pack "    MONDAY = 0,"
            , T.pack "    TUESDAY,"
            , T.pack "    WEDNESDAY,"
            , T.pack "    THURSDAY,"
            , T.pack "    FRIDAY,"
            , T.pack "    SATURDAY,"
            , T.pack "    SUNDAY,"
            , T.pack "}"
            ]
    
    putStrLn "Example input:"
    TIO.putStrLn exampleInput
    
    case parseEnumFile exampleInput of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right enums -> do
            putStrLn "Parsed successfully!"
            putStrLn $ "Found " ++ show (length enums) ++ " enum definitions"
            
            let (cppCode, csharpCode) = generateAll enums
            
            putStrLn "\n=== Generated C++ Code ==="
            TIO.putStrLn cppCode
            
            putStrLn "\n=== Generated C# Code ==="
            TIO.putStrLn csharpCode
