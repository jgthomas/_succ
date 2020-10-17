-- |
-- Module       : Optimiser
-- Description  : Optimise assembly schema
--
-- Optimises assembly schema to reduce assembly code size
module Optimiser.Optimiser
  ( optimise,
  )
where

import Optimiser.OptimiserExpression (optimiseExpression)
import Types.AssemblySchema

-- | Optimises an assembly schema
optimise :: AssemblySchema -> AssemblySchema
optimise schema@ProgramSchema {} = schema
optimise schema@FunctionSchema {} = schema
optimise schema@DeclarationSchema {} = schema
optimise schema@StatementSchema {} = schema
optimise (ExpressionSchema schema) = ExpressionSchema (optimiseExpression schema)
optimise schema@SkipSchema {} = schema
