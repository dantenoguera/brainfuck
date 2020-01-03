{
module Parse where
import Common
import Data.Maybe
import Data.Char
}

%monad { P } { thenP } { returnP }
%name parseExp Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '>'     {TGreater}
    '<'     {TLess}
    '+'     {TPlus}
    '-'     {TMinus}
    '.'     {TDot}
    ','     {TComma}
    '['     {TLBracket}
    ']'     {TRBracket}

%%

Exp :   Op Exp          { $1 : $2 }
    |                   { [] }

Op  :   '[' Exp ']'     { While $2 }
    |    '>'            { IncP }
    |    '<'            { DecP }
    |    '+'            { IncB }
    |    '-'            { DecB }
    |    '.'            { W }
    |    ','            { R }


{

data ParseResult a = Ok a | Failed String
                     deriving Show
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea " ++ (show (i::LineNumber)) ++ ": Error de parseo\n"++(s)

data Token = TGreater
           | TLess
           | TPlus
           | TMinus
           | TDot
           | TComma
           | TLBracket
           | TRBracket
           | TEOF
            deriving Show

----------------------------------

lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)                    
                    ('>' : cs) -> cont TGreater cs
                    ('<' : cs) -> cont TLess cs
                    ('+' : cs) -> cont TPlus cs
                    ('-' : cs) -> cont TMinus cs
                    ('.' : cs) -> cont TDot cs
                    (',' : cs) -> cont TComma cs
                    ('[' : cs) -> cont TLBracket cs
                    (']' : cs) -> cont TRBracket cs
                    (_ : cs) -> lexer cont cs
              
exp_parse s = parseExp s 1
}














