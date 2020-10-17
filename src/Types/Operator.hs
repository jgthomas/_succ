{-# LANGUAGE DeriveDataTypeable #-}

module Types.Operator where

import Data.Data (Data)

data Operator
  = Assignment
  | BinaryOp BinaryOp
  | UnaryOp UnaryOp
  deriving (Show, Eq, Data)

data BinaryOp
  = Plus
  | Minus
  | Divide
  | Multiply
  | Modulo
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GThanOrEqu
  | LThanOrEqu
  | LogicalOR
  | LogicalAND
  | BitwiseXOR
  | BitwiseAND
  | BitwiseOR
  | ShiftOp ShiftOp
  deriving (Show, Eq, Data)

data ShiftOp
  = LeftShift
  | RightShift
  deriving (Show, Eq, Data)

data UnaryOp
  = Unary Unary
  | PreOpUnary PreOpUnary
  | PostOpUnary PostOpUnary
  deriving (Show, Eq, Data)

data Unary
  = Negate
  | Positive
  | BitwiseComp
  | LogicalNeg
  deriving (Show, Eq, Data)

data PreOpUnary
  = PreIncrement
  | PreDecrement
  deriving (Show, Eq, Data)

data PostOpUnary
  = PostIncrement
  | PostDecrement
  deriving (Show, Eq, Data)
