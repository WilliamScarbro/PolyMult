module Compile.CAST where

data CProgram = CProgram [CStatement]

-- C AST

data CStatement
  = CAssignment Identifier CExpr
  -- | If CExpr [tatement] (Maybe [Statement])
  -- | While CExpr [Statement]
  -- | Return (Maybe CExpr)

data CExpr
  = CBinaryOp CBinaryOperator CExpr CExpr
  | CUnaryOp CUnaryOperator CExpr
  | Literal Literal
  | Identifier String

data CBinaryOperator
  = CAdd
  | CSubtract
  | CMultiply
  | CDivide
  | CModulo
--  | Equal
--  | NotEqual
--  | LessThan
--  | LessThanOrEqual
--  | GreaterThan
--  | GreaterThanOrEqual
--  | And
--  | Or

data CUnaryOperator
  = CNegate
  | CNot

data Literal
  = IntLiteral Integer
  | FloatLiteral Double
  | BoolLiteral Bool
  | CharLiteral Char

type Identifier = String

data COperator = CBinary CBinaryOperator | CUnary CUnaryOperator

--

translateCToStr :: CProgram -> String
translateCToStr (CProgram stmts) = unlines (map translateStmt stmts)

translateStmt :: CStatement -> String
translateStmt (CAssignment ident expr) = ident ++ " = " ++ translateExpr expr ++ ";"
-- translate If, While, Return statements


translateOp :: COperator -> String
translateOp (CBinary op) = translateBinaryOp op
translateOp (CUnary op) = translateUnaryOp op

translateBinaryOp :: CBinaryOperator -> String
translateBinaryOp CAdd = "+"
translateBinaryOp CSubtract = "-"
translateBinaryOp CMultiply = "*"
translateBinaryOp CDivide = "/"
translateBinaryOp CModulo = "%"

translateUnaryOp :: CUnaryOperator -> String
translateUnaryOp CNegate = "-"
translateUnaryOp CNot = "!"

translateExpr :: CExpr -> String
translateExpr (CBinaryOp op left right) = "(" ++ translateExpr left ++ " " ++ translateOp (CBinary op) ++ " " ++ translateExpr right ++ ")"
translateExpr (CUnaryOp op expr) = translateOp (CUnary op) ++ "(" ++ translateExpr expr ++ ")"
translateExpr (Literal (IntLiteral i)) = show i
translateExpr (Literal (FloatLiteral f)) = show f
translateExpr (Literal (BoolLiteral b)) = if b then "true" else "false"
translateExpr (Literal (CharLiteral c)) = show c
translateExpr (Identifier s) = s

translateLiteral :: Literal -> String
translateLiteral (IntLiteral n) = show n
translateLiteral (FloatLiteral x) = show x
translateLiteral (BoolLiteral b) = if b then "true" else "false"
translateLiteral (CharLiteral c) = ['\'', c, '\'']
