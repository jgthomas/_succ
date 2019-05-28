
module Scopes (LocalScope, FunctionScope, ProgramScope) where


import qualified Data.Map as M


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
type ProgramScope = M.Map String FunctionScope
