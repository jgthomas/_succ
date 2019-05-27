
module AST (Tree(..)) where


import Tokens (Operator(..))


data Tree = ProgramNode [Tree]
          | FunctionNode String [Tree] [Tree]
          | FunctionProtoNode String [Tree]
          | DeclarationNode String (Maybe Tree)
          | CompoundStmtNode [Tree]               -- statements
          | ReturnNode Tree
          | AssignmentNode String Tree Operator
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
          | UnaryNode Tree Operator
          | BinaryNode Tree Tree Operator
          | TernaryNode Tree Tree Tree
          | AssignNode String Tree
          deriving Show
