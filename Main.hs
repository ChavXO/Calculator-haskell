module Main where

import Graphics.UI.Gtk
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Monad.Except
import Control.Monad.Identity

-- Lexer

def = emptyDef { identStart  = letter
               , identLetter = alphaNum 
               , opStart     = oneOf "+-*/="
               , opLetter    = oneOf "+-*/="
               }

lexer :: TokenParser ()
lexer = makeTokenParser def

-- Expression tree

data Expression = Constant Double
                | Identifier String
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Negation Expression
                | Assignment Expression Expression
                deriving Show

-- Parser

parseNumber :: Parser Expression
parseNumber = do
    v <- naturalOrFloat lexer
    case v of
        Left  i -> return $ Constant $ fromIntegral i
        Right n -> return $ Constant n

parseIdentifier :: Parser Expression
parseIdentifier = do
   i <- identifier lexer
   return $ Identifier i
   
parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm [
   [ Prefix (reservedOp lexer "-" >> return Negation)
   , Prefix (reservedOp lexer "+" >> return id) 
   ]
 , [ Infix (reservedOp lexer "*" >> return Multiplication) AssocLeft
   , Infix (reservedOp lexer "/" >> return Division) AssocLeft
   ]
 , [ Infix (reservedOp lexer "+" >> return Addition) AssocLeft
   , Infix (reservedOp lexer "-" >> return Subtraction) AssocLeft 
   ]
 , [ Infix (reservedOp lexer "=" >> return Assignment) AssocRight
   ]
 ]

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression 
        <|> parseNumber
        <|> parseIdentifier

parseInput :: Parser Expression
parseInput = do
    whiteSpace lexer
    ex <- parseExpression
    eof
    return ex

-- Evaluator

type SymTab = M.Map String Double

type Evaluator a = S.StateT SymTab (ExceptT String Identity) a

runEvaluator :: Evaluator Double -> SymTab -> Either String (Double, SymTab)
runEvaluator calc symTab = runIdentity $ runExceptT $ S.runStateT calc symTab

eval :: Expression -> Evaluator Double

eval (Constant x) = return x

eval (Identifier i) = do
    symtab <- S.get
    case M.lookup i symtab of
        Nothing -> fail $ "Undefined variable " ++ i
        Just e  -> return e

eval (Addition eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft + rgt

eval (Subtraction eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft - rgt

eval (Multiplication eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft * rgt

eval (Division eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft / rgt

eval (Negation e) = do
    val <- eval e
    return $ -val

eval (Assignment (Identifier i) e) = do
    val <- eval e
    S.modify (M.insert i val)
    return val

eval (Assignment _ _) =
    fail "Left of assignment must be an identifier"

defaultVars :: M.Map String Double
defaultVars = M.fromList
   [ ("e", exp 1)
   , ("pi", pi)
   ]
   
--runEvaluator returns Either String (Double, SymTab Double)

calculate :: SymTab -> String -> (String, SymTab)
calculate symTab s = 
    case parse parseInput "" s of
    Left  err -> ("error: " ++ (show err), symTab)
    Right exp -> case runEvaluator (eval exp) symTab of
                 Left  err              -> ("error: " ++ err, symTab)
                 Right (val, newSymTab) -> (show val, newSymTab)
                 
evaluate myEntry = do
    line <- entryGetText myEntry
    if null line
    then return ()
    else do
        let (result, symTab') = calculate defaultVars line
        entrySetText myEntry result
    
    
processClick text op = do
    let newtxt = addToText op
    entryAppendText text newtxt
   
addToText str
    | isDigit str = str
    | isSymbol str = " " ++ str ++ " "
    | otherwise = ""
    
symbols = ["+","-","/","*","(",")"]
digits = ["0","1","2","3","4","5","6","7","8", "9"]
isSymbol str = elem str symbols
isDigit str = elem str digits


main = do
    initGUI
    window <- windowNew
    vbox       <- vBoxNew False 0
    set window [containerBorderWidth := 10,
              windowTitle := "Calculator",
              windowDefaultWidth := 400, windowDefaultHeight := 500,
              containerChild := vbox]
    textView <- entryNew
    srcfont <- fontDescriptionFromString "Calibri Normal 48"
    widgetModifyFont textView (Just srcfont)
    entrySetAlignment textView 1
    set textView [widgetHeightRequest := 100]
    boxPackStart vbox textView PackGrow 0
    table <- tableNew 7 5 True
    boxPackStart vbox table PackGrow 0 
    buttonSqrd <- buttonNewWithLabel "x^2"
    --onClicked button1 (processClick button1)
    tableAttachDefaults table buttonSqrd 0 1 0 1
    buttonPow <- buttonNewWithLabel "x^y"
    --onClicked button2 (processClick button2)
    tableAttachDefaults table buttonPow 1 2 0 1
    buttonSin <- buttonNewWithLabel "sin"
    --onClicked button3 mainQuit
    tableAttachDefaults table buttonSin 2 3 0 1
    buttonCos <- buttonNewWithLabel "cos"
    tableAttachDefaults table buttonCos 3 4 0 1
    buttonTan <- buttonNewWithLabel "tan"
    tableAttachDefaults table buttonTan 4 5 0 1
    
    buttonSqrt <- buttonNewWithLabel "sqrt"
    --onClicked button1 (processClick button1)
    tableAttachDefaults table buttonSqrt 0 1 1 2
    buttonPow10 <- buttonNewWithLabel "10^x"
    --onClicked button2 (processClick button2)
    tableAttachDefaults table buttonPow10 1 2 1 2
    buttonLog <- buttonNewWithLabel "log"
    --onClicked button3 mainQuit
    tableAttachDefaults table buttonLog 2 3 1 2
    buttonExp <- buttonNewWithLabel "Exp"
    tableAttachDefaults table buttonExp 3 4 1 2
    buttonMod <- buttonNewWithLabel "mod"
    tableAttachDefaults table buttonMod 4 5 1 2
    
    button2nd <- buttonNewWithLabel "2nd"
    --onClicked button1 (processClick button1)
    tableAttachDefaults table button2nd 0 1 2 3
    buttonCE <- buttonNewWithLabel "CE"
    onClicked buttonCE (entrySetText textView "")
    tableAttachDefaults table buttonCE 1 2 2 3
    buttonC <- buttonNewWithLabel "C"
    onClicked buttonC (entrySetText textView "")
    tableAttachDefaults table buttonC 2 3 2 3
    buttonDel <- buttonNewWithLabel "<-"
    tableAttachDefaults table buttonDel 3 4 2 3
    buttonDiv <- buttonNewWithLabel "/"
    onClicked buttonDiv (processClick textView "/")
    tableAttachDefaults table buttonDiv 4 5 2 3
    
    buttonPI <- buttonNewWithLabel "pi"
    --onClicked button1 (processClick button1)
    tableAttachDefaults table buttonPI 0 1 3 4
    button7 <- buttonNewWithLabel "7"
    onClicked button7 (processClick textView "7")
    tableAttachDefaults table button7 1 2 3 4
    button8 <- buttonNewWithLabel "8"
    onClicked button8 (processClick textView "8")
    tableAttachDefaults table button8 2 3 3 4
    button9 <- buttonNewWithLabel "9"
    onClicked button9 (processClick textView "9")
    tableAttachDefaults table button9 3 4 3 4
    buttonMult <- buttonNewWithLabel "*"
    onClicked buttonMult (processClick textView "*")
    tableAttachDefaults table buttonMult 4 5 3 4
    
    buttonFact <- buttonNewWithLabel "n!"
    --onClicked button1 (processClick button1)
    tableAttachDefaults table buttonFact 0 1 4 5
    button4 <- buttonNewWithLabel "4"
    onClicked button4 (processClick textView "4")
    tableAttachDefaults table button4 1 2 4 5
    button5 <- buttonNewWithLabel "5"
    onClicked button5 (processClick textView "5")
    tableAttachDefaults table button5 2 3 4 5
    button6 <- buttonNewWithLabel "6"
    onClicked button6 (processClick textView "6")
    tableAttachDefaults table button6 3 4 4 5
    buttonMinus <- buttonNewWithLabel "-"
    onClicked buttonMinus (processClick textView "-")
    tableAttachDefaults table buttonMinus 4 5 4 5
    
    buttonSign <- buttonNewWithLabel "+/-"
    --onClicked button1 (processClick button1)
    tableAttachDefaults table buttonSign 0 1 5 6
    button1 <- buttonNewWithLabel "1"
    onClicked button1 (processClick textView "1")
    tableAttachDefaults table button1 1 2 5 6
    button2 <- buttonNewWithLabel "2"
    onClicked button2 (processClick textView "2")
    tableAttachDefaults table button2 2 3 5 6
    button3 <- buttonNewWithLabel "3"
    onClicked button3 (processClick textView "3")
    tableAttachDefaults table button3 3 4 5 6
    buttonAdd <- buttonNewWithLabel "+"
    onClicked buttonAdd (processClick textView "+")
    tableAttachDefaults table buttonAdd 4 5 5 6
    
    buttonLBrc <- buttonNewWithLabel "("
    onClicked buttonLBrc (processClick textView "(")
    tableAttachDefaults table buttonLBrc 0 1 6 7
    buttonRBrc <- buttonNewWithLabel ")"
    onClicked buttonRBrc (processClick textView ")")
    tableAttachDefaults table buttonRBrc 1 2 6 7
    button0 <- buttonNewWithLabel "0"
    onClicked button0 (processClick textView "0")
    tableAttachDefaults table button0 2 3 6 7
    buttonPoint <- buttonNewWithLabel "."
    onClicked buttonPoint (processClick textView ".")
    tableAttachDefaults table buttonPoint 3 4 6 7
    buttonEq <- buttonNewWithLabel "="
    tableAttachDefaults table buttonEq 4 5 6 7
    onClicked buttonEq (evaluate textView)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
