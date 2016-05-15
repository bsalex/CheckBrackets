module CheckBrackets where
    import Data.Foldable
    import System.Environment

    main = do
        args <- getArgs
        case args of
            (x:xs) -> putStrLn (formatCheckResult (checkExpression (unwords (x:xs))))
            _ -> do
                    putStrLn "Please enter an expression to check:"
                    expression <- getLine
                    putStrLn (formatCheckResult (checkExpression expression))

    formatCheckResult :: Bool -> String
    formatCheckResult True = "\ESC[0;32mThe expression is valid\ESC[0m"
    formatCheckResult False = "\ESC[0;31mThe expression is invalid\ESC[0m"

    checkExpression :: String -> Bool
    checkExpression "" = True
    checkExpression (x:xs) = checkExpressionWithStack (x:xs) ""

    pairs :: [(String, String)]
    pairs = [("(", ")"), ("[", "]"), ("{", "}")]

    checkExpressionWithStack :: String -> String -> Bool
    checkExpressionWithStack "" "" = True
    checkExpressionWithStack "" (stackFirstSymbol:stack) = False

    checkExpressionWithStack (expressionFirstSymbol:expression) "" = case (findPair pairs [expressionFirstSymbol]) of
        Nothing -> checkExpressionWithStack expression ""
        Just pair | snd pair == [expressionFirstSymbol] -> False
        Just pair | fst pair == [expressionFirstSymbol] -> checkExpressionWithStack expression [expressionFirstSymbol]

    checkExpressionWithStack (expressionFirstSymbol:expression) (stackFirstSymbol:stack) = case (findPair pairs [expressionFirstSymbol]) of
        Nothing -> checkExpressionWithStack expression (stackFirstSymbol:stack)
        Just pair | pair == ([stackFirstSymbol], [expressionFirstSymbol]) -> checkExpressionWithStack expression stack
        Just pair | snd pair == [expressionFirstSymbol] -> False
        Just pair | fst pair == [expressionFirstSymbol] -> checkExpressionWithStack expression (expressionFirstSymbol:stackFirstSymbol:stack)


    findPair :: [(String, String)] -> String -> Maybe (String, String)
    findPair pairs pair = find (\(open, close) -> open == pair || close == pair) pairs
