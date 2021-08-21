-- A Script That Can Calculate Results
-- Of Reverse Polish Notation Expressions
-- Eg:
-- 10 5 3 + 2 * -
-- >> 6

module Main (main) where

    main :: IO ()
    main = do
        putStrLn "Enter Reverse Polish Notation To Calculate: "
        notation <- getLine
        putStrLn (show (opsSolve [] notation))

    opsSolve :: [Int] -> String -> [Int]
    opsSolve s [] = s
    opsSolve (firstOnStack : secondOnStack : restOfStack) reversePolishNotation
        | polishOp == "+" = opsSolve (secondOnStack   +   firstOnStack : restOfStack) stringRestOfNotation
        | polishOp == "-" = opsSolve (secondOnStack   -   firstOnStack : restOfStack) stringRestOfNotation
        | polishOp == "*" = opsSolve (secondOnStack   *   firstOnStack : restOfStack) stringRestOfNotation
        | polishOp == "/" = opsSolve (secondOnStack `div` firstOnStack : restOfStack) stringRestOfNotation
        where (polishOp : restOfNotation) = words reversePolishNotation
              stringRestOfNotation = unwords restOfNotation

    opsSolve wholeStack reversePolishNotation = opsSolve (firstNum : wholeStack) notationButFirst
        where wholeNotation = words reversePolishNotation
              notationButFirst = unwords (tail wholeNotation)
              firstNum = read (head wholeNotation) :: Int

-- Written By @Abdul-Muiz-Iqbal
