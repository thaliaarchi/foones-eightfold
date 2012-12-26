{
module Parser(parse) where

import Char

import Lang

data Token = TokenColon
           | TokenFun
           | TokenId Id
           | TokenLParen
           | TokenRParen
           | TokenDot
           | TokenComma
           | TokenProof
           | TokenAskType
           | TokenAskValue
           deriving (Show)

}

%name parseTokens
%tokentype { Token }
%error { parseError }
%monad { M }

%token
    colon    { TokenColon }
    fun      { TokenFun }
    id       { TokenId $$ }
    lparen   { TokenLParen }
    rparen   { TokenRParen }
    comma    { TokenComma }
    dot      { TokenDot }
    proof    { TokenProof }
    askType  { TokenAskType }
    askValue { TokenAskValue }

%%

Program
    : {- empty -}                           { [] }
    | id colon Exp dot Program              { Assume $1 $3 : $5 }
    | id colon Exp proof Exp dot Program    { Prove $1 $3 $5 : $7 }
    | id proof Exp dot Program              { Define $1 $3 : $5 }
    | askType Exp dot Program               { AskType $2 : $4 }
    | askValue Exp dot Program              { AskValue $2 : $4 }

Exp
    : colon VarTypeList dot Exp       { foldr (uncurry Lam) $4 $2 }
    | fun Atom Exp                    { Lam "_" $2 $3 }
    | AppExp                          { $1 }

AppExp
    : Atom                          { $1 }
    | AppExp Atom                   { App $1 $2 }

VarTypeList
    : id AppExp                      { [($1, $2)] }
    | VarTypeList comma id AppExp    { $1 ++ [($3, $4)] }

Atom
    : id                  { Var $1 }
    | lparen Exp rparen   { $2 }

{

parseError :: [Token] -> M a
parseError msg = fail "parse error"

isBlank :: Char -> Bool 
isBlank x = x `elem` " \n\t\r"

isSymbol :: Char -> Bool 
isSymbol x = x `elem` "_+-*"

tokenize :: String -> [Token]
tokenize (x:xs)
  | isBlank x = tokenize xs
tokenize (x:xs)
  | isLower x || isSymbol x = TokenId [x] : tokenize xs
tokenize (x:xs)
  | isDigit x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p  = isDigit
tokenize (x:xs)
  | isUpper x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p  = isLower
tokenize ('#':xs)           = tokenize ys
  where ys = dropWhile (/= '\n') xs
tokenize ('=':xs)           = TokenProof : tokenize xs
tokenize (':':xs)           = TokenColon : tokenize xs
tokenize ('.':xs)           = TokenDot : tokenize xs
tokenize (',':xs)           = TokenComma : tokenize xs
tokenize ('(':xs)           = TokenLParen : tokenize xs
tokenize (')':xs)           = TokenRParen : tokenize xs
tokenize ('>':xs)           = TokenFun : tokenize xs
tokenize ('?':xs)           = TokenAskType : tokenize xs
tokenize ('!':xs)           = TokenAskValue : tokenize xs
tokenize [] = []

parse :: String -> M Program
parse = parseTokens . tokenize

}
