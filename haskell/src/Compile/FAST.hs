module FAST where

-- Field AST

data FieldOp
  = FAdd FieldExpr FieldExpr
  | FSubtract FieldExpr FieldExpr
  | FMultiply FieldExpr FieldExpr
  | FDivide FieldExpr FieldExpr
  | FNthRootOfUnity Int

data FieldExpr a
  = Constant a
  | Variable String
  | FieldOpExpr FieldOp

type FieldAST a = FieldExpr a


-- translation

class Field a where
  add :: a -> CExpr -> CExpr -> CExpr
  sub :: a -> CExpr -> CExpr -> CExpr
  mult :: a -> CExpr -> CExpr -> CExpr
  div :: a -> CExpr -> CExpr -> CExpr
