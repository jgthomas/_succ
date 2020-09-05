
module TestUtility (mockNodeDat) where


import Types.AST (NodeDat (..))


dummyLine :: Int
dummyLine = 0


mockNodeDat :: NodeDat
mockNodeDat = NodeDat dummyLine dummyLine False False 1
