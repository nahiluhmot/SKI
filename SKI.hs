import System.IO
import System.Environment
import Control.Monad (mapM)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec


data SKI = S | K | I | List [SKI] | Atom String

instance Show SKI where
    show S         = "s"
    show K         = "k"
    show I         = "i"
    show (Atom st) = '\'' : st
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"

-- parsers
runParse :: String -> Either String SKI
runParse input = case parse parseSKI "ski" input of
                     Left err  -> Left $ show err
                     Right val -> Right val

parseSKI :: Parser SKI
parseSKI = parsePrim <|> parseList <|> parseAtom

parseAtom :: Parser SKI
parseAtom = do
    char '\''
    c <- many1 letter
    return $ Atom c

parsePrim :: Parser SKI
parsePrim = (char 's' >> return S)
        <|> (char 'k' >> return K)
        <|> (char 'i' >> return I)

parseList :: Parser SKI
parseList = do
    char '('
    spaces
    xs <- sepBy1 parseSKI spaces
    spaces
    char ')'
    return (List xs)
    
eval :: SKI -> Either String SKI
-- S, K, and I all evaluate to themselves
eval S                = return $ S
eval K                = return $ K
eval I                = return $ I
-- so does an atom
eval (Atom str)       = return $ Atom str
-- so does a list that begins with an atom
eval (List (a@(Atom _):xs)) = do
    ys <- mapM eval xs
    return $ List (a:ys)
-- cases for perfect number of args
eval (List [I,x])     = eval x
eval (List [K,x,y])   = eval x
eval (List [S,f,g,x]) = do g' <- eval g
                           f' <- eval f
                           x' <- eval x
                           eval $ List [g', x', List [f', x']]
-- cases for no args
eval (List [S]) = return S
eval (List [K]) = return K
eval (List [I]) = return I
eval (List []) = return (List [])
-- cases for not enough args
eval (List [S,x,y]) = do
    x' <- eval x
    y' <- eval y
    return $ List [S, x', y']
eval (List [S,x])   = do
    x' <- eval x
    return $ List [S, x']
eval (List [K,x])   = do
    x' <- eval x
    return $ List [K, x']
-- cases for too many args
eval (List (S:x:y:z:a:bs)) = do
    x' <- eval $ List [S,x,y,z]
    a' <- eval a
    bs' <- mapM eval bs
    eval $ List (x':a':bs')
eval (List (K:x:y:z:as)) = do
    x' <- eval $ List [K,x,y]
    z' <- eval z
    as' <- mapM eval as
    eval $ List (x':z':as')
eval (List (I:x:y:zs)) = do
    x' <- eval $ List [I,x] 
    y' <- eval y
    zs' <- mapM eval zs
    eval $ List (x':y':zs')
eval (List (xs@(List _):ys)) = do
    xs' <- eval xs
    ys' <- mapM eval ys
    case xs' of
        (List zs) -> eval . List $ zs ++ ys
        zs -> eval . List $ zs:ys'
-- throw bad form errors otherwise
--eval x = Left $ "Bad form: " ++ show x

readEvalShow :: String -> String
readEvalShow input = let showIt (Right val) = "=> " ++ show val
                         showIt (Left err)  = err
                     in  showIt $ do parsed <- runParse input
                                     evaled <- eval parsed
                                     return evaled

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Welcome to SKI!" >> loop
        else (fmap readEvalShow . readFile $ head args) >>= putStrLn


loop :: IO ()
loop = do
    putStr "> "
    hFlush stdout
    line <- getLine
    putStrLn $ readEvalShow line
    loop
