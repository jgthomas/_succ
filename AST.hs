
module AST (Tree(..)) where


import Tokens (Operator)
import Types  (Type)


data Tree = ProgramNode [Tree]
          | FunctionNode String [Tree] [Tree]
          | FunctionProtoNode String [Tree]
          | DeclarationNode String (Maybe Tree)
          | PointerNode String (Maybe Tree)
          | CompoundStmtNode [Tree]               -- statements
          | ReturnNode Tree
          | AssignmentNode String Tree Operator
          | AssignDereferenceNode String Tree Operator
          | ExprStmtNode Tree
          | IfNode Tree Tree (Maybe Tree)
          | WhileNode Tree Tree
          | DoWhileNode Tree Tree
          | ForLoopNode Tree Tree Tree Tree
          | BreakNode
          | ContinueNode
          | ConstantNode Int                      -- expressions
          | FuncCallNode String [Tree]
          | ParamNode Tree
          | ArgNode Tree
          | NullExprNode
          | VarNode String
          | AddressOfNode String
          | DereferenceNode String
          | UnaryNode Tree Operator
          | BinaryNode Tree Tree Operator
          | TernaryNode Tree Tree Tree
          deriving (Show)
