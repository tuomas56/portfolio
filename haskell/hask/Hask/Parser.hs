{-# LANGUAGE LambdaCase #-}

module Hask.Parser where
    
import ParserCombinators
import Hask.AST
import Debug.Trace
import Data.Bifunctor

spaces :: Parser Char ()
spaces = many (unit ' ') <>> pure ()

intNumber :: (Eq a, Read a, Num a) => Parser Char a
intNumber = onePlus digit ~> read
    where digit = choice $ map unit ['0'..'9']

parseIdent :: Parser Char String
parseIdent = opIdent <|> stdIdent
    where letter = choice $ map unit ['a'..'z']
          digit = choice $ map unit ['0'..'9']
          special = choice $ map unit "+-/*~$:,^&!@><=|/\\"
          other = choice $ map unit "[]"
          stdIdent = (letter <|> other <|> unit '_') <+> many (letter <|> digit <|> other) ~> uncurry (:)
          opIdent = unit '(' <>> many special <<> unit ')'

parseName :: Parser Char Name
parseName = parseIdent ~> Name

parseArgName :: Parser Char ArgName
parseArgName = parseIdent ~> ArgName

parseOperator :: Parser Char Expr
parseOperator = onePlus special ~> ArgName ~> Arg
    where special = choice $ map unit "+-/*~$:"

parseCall :: Parser Char Expr
parseCall = do
    callee <- parseNotPost
    unit ' '
    spaces
    arg <- parseExpr
    return $ transform (Call callee arg)
    where transform (Call x (Call y z)) = Call (Call (transform x) (transform y)) (transform z)
          transform (LetExpr e n b) = LetExpr (transform e) n (transform b)
          transform (LambdaExpr a b) = LambdaExpr a (transform b)
          transform (Brackets e) = Brackets $ transform e
          transform (CaseExpr e bs) = CaseExpr (transform e) (map (bimap id transform) bs)
          transform x = x

parseBinOp :: Parser Char Expr
parseBinOp = do
    lhs <- parseNotPost
    spaces
    op <- parseOperator
    spaces
    rhs <- parseExpr
    return $ Call (Call op lhs) rhs

parseLiteral :: Parser Char Expr
parseLiteral = intNumber ~> NumLiteral

parseArg :: Parser Char Expr
parseArg = parseArgName ~> Arg

parseLetExpr :: Parser Char Expr
parseLetExpr = do
    string "let"
    spaces
    name <- parseName
    spaces
    unit '='
    spaces
    value <- parseExpr
    spaces
    string "in"
    spaces
    body <- parseExpr
    return $ LetExpr value name body

parseBrackets :: Parser Char Expr
parseBrackets = do
    unit '('
    val <- parseExpr
    unit ')'
    return $ Brackets val

parsePattern :: Parser Char Pattern
parsePattern =  nilPattern 
            <|> consPattern 
            <|> identPattern
            <|> constantPattern
    where nilPattern = string "[]" ~> const NilPattern
          consPattern = do
              unit '('
              h <- parsePattern
              unit ':'
              t <- parsePattern
              unit ')'
              return $ ConsPattern h t
          identPattern = parseIdent ~> Name ~> IdentPattern
          constantPattern = intNumber ~> Number ~> ConstantPattern

parseCaseExpr :: Parser Char Expr
parseCaseExpr  = do
    string "case"
    spaces
    val <- parseNotPost
    spaces
    string "of"
    spaces
    unit '{'
    opt $ unit '\n'
    branch <- do
        spaces
        pat <- parsePattern
        spaces 
        string "->"
        spaces
        val <- parseExpr
        return (pat, val)
    branches <- many $ do
        terminator
        spaces
        pat <- parsePattern
        spaces
        string "->"
        spaces
        val <- parseExpr
        spaces
        return (pat, val)
    spaces
    opt $ unit '\n'
    unit '}'
    return $ CaseExpr val $ branch:branches

parseNotPost :: Parser Char Expr
parseNotPost =  parseBrackets
            <|> parseArg
            <|> parseLiteral

parseExpr :: Parser Char Expr
parseExpr =  parseCaseExpr
         <|> parseLetExpr
         <|> parseBinOp
         <|> parseCall
         <|> parseNotPost

parseConstantDecl :: Parser Char Decl
parseConstantDecl = do
    name <- parseName
    spaces
    unit '='
    spaces
    expr <- parseExpr
    return $ ConstantDecl name expr

parseFunctionDecl :: Parser Char Decl
parseFunctionDecl = do
    name <- parseName
    spaces
    args <- onePlus $ parsePattern <<> spaces
    unit '='
    spaces
    expr <- parseExpr
    return $ ConstantDecl name $ thread expr args
    where thread expr [] = expr
          thread expr (x:xs) = LambdaExpr (ArgName "`_") $ CaseExpr (Arg (ArgName "`_")) [(x, thread expr xs)]

parseDecl :: Parser Char Decl
parseDecl = parseFunctionDecl <|> parseConstantDecl

terminator :: Parser Char Char
terminator =  (spaces <>> unit ';' <<> spaces)
          <|> (unit '\n')

parseProgram :: Parser Char Program
parseProgram = opt parseDecl <+> many (terminator <>> parseDecl)
    ~> \case
        (Just x, []) -> [x]
        (Nothing, []) -> []
        (Just x, y) -> x:y
        (Nothing, y) -> y
    ~> Program

readProgram :: String -> Parse Program
readProgram = run parseProgram
