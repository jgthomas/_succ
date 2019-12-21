
module AST where


import Operator (BinaryOp, UnaryOp)
import Type     (Type)


data Tree = ProgramNode [Tree]
          | FunctionNode Type String [Tree] (Maybe [Tree])
          | DeclarationNode String Type (Maybe Tree)
          | PointerNode String Type (Maybe Tree)
          | CompoundStmtNode [Tree]               -- statements
          | ReturnNode Tree
          | AssignmentNode String Tree BinaryOp
          | AssignDereferenceNode String Tree BinaryOp
          | ExprStmtNode Tree
          | IfNode Tree Tree (Maybe Tree)
          | WhileNode Tree Tree
          | DoWhileNode Tree Tree
          | ForLoopNode Tree Tree Tree Tree
          | BreakNode
          | ContinueNode
          | ConstantNode Int                      -- expressions
          | FuncCallNode String [Tree]
          | ParamNode Type Tree
          | ArgNode Tree
          | NullExprNode
          | VarNode String
          | AddressOfNode String
          | DereferenceNode String
          | UnaryNode Tree UnaryOp
          | BinaryNode Tree Tree BinaryOp
          | TernaryNode Tree Tree Tree
          deriving (Show, Eq)
