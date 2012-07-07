import Control.Applicative 
import Control.Monad

-- Cues from 'Functional Pearl: Monadic Parsing in Haskell'

newtype Parser p = Parser (String -> [(p, String)])

parse :: Parser p -> String -> [(p, String)]
parse (Parser p) = p

instance Functor Parser where
    fmap f p = pure f <*> p

instance Applicative Parser where
    pure a = Parser (\cs -> [(a, cs)])
    (<*>)  = ap

instance Monad Parser where
    return  = pure
    p >>= f = Parser (\cs -> concat [parse (f a) cs0 | (a, cs0) <- parse p cs]) 

item :: Parser Char
item = Parser (\cs -> case cs of 
                        ""     -> []            -- subsumes `eof`
                        (c:cs) -> [(c, cs)]) 





