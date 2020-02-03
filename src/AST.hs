
module AST where


import Operator (BinaryOp, Operator, UnaryOp)
import Type     (Type)


data Tree = ProgramNode [Tree]
          | FunctionNode Type String [Tree] (Maybe [Tree]) NodeDat
          | DeclarationNode String Type (Maybe Tree) NodeDat
          | PointerNode String Type (Maybe Tree) NodeDat
          | CompoundStmtNode [Tree] NodeDat
          | ReturnNode Tree NodeDat
          | AssignmentNode String Tree Operator NodeDat
          | AssignDereferenceNode String Tree Operator NodeDat
          | ExprStmtNode Tree
          | IfNode Tree Tree (Maybe Tree)
          | WhileNode Tree Tree
          | DoWhileNode Tree Tree
          | ForLoopNode Tree Tree Tree Tree
          | BreakNode NodeDat
          | ContinueNode NodeDat
          | ConstantNode Int
          | FuncCallNode String [Tree]
          | ParamNode Type Tree NodeDat
          | ArgNode Tree
          | NullExprNode NodeDat
          | VarNode String
          | AddressOfNode String
          | DereferenceNode String
          | UnaryNode Tree UnaryOp
          | BinaryNode Tree Tree BinaryOp
          | TernaryNode Tree Tree Tree
          deriving (Show, Eq)


data NodeDat = NodeDat { startLine :: Int
                       , endLine   :: Int }
             deriving (Show, Eq)


mkNodeDat :: Int -> Int -> NodeDat
mkNodeDat n m = NodeDat n m
