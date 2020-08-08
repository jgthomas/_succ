
module Builder.SchemaCheck
        (getFunctions,
         getInitialisedInt,
         getUninitialised,
         getPointersAssignmentsForInit
        ) where


import Types.AssemblySchema


getFunctions :: [AssemblySchema] -> [AssemblySchema]
getFunctions items = filter isFunction items


getInitialisedInt :: [AssemblySchema] -> [AssemblySchema]
getInitialisedInt items = filter isInitialisedInt items


getUninitialised :: [AssemblySchema] -> [AssemblySchema]
getUninitialised items = map convertForInit . filter needsInit $ items


getPointersAssignmentsForInit :: [AssemblySchema] -> [AssemblySchema]
getPointersAssignmentsForInit items = filter isInitialisedPointer items


isFunction :: AssemblySchema -> Bool
isFunction FunctionSchema{} = True
isFunction _                = False


isInitialisedInt :: AssemblySchema -> Bool
isInitialisedInt (DeclarationSchema
                  _
                  (StatementSchema (AssignmentSchema _ (ExpressionSchema AddressOfSchema{}) _))
                  _
                  _
                 )                                    = False
isInitialisedInt (DeclarationSchema _ SkipSchema _ _) = False
isInitialisedInt DeclarationSchema{}                  = True
isInitialisedInt _                                    = False


needsInit :: AssemblySchema -> Bool
needsInit (DeclarationSchema _ SkipSchema _ _) = True
needsInit schema                               = isInitialisedPointer schema


convertForInit :: AssemblySchema -> AssemblySchema
convertForInit schema@(DeclarationSchema _ SkipSchema _ _) = schema
convertForInit (DeclarationSchema
                varSchema
                (StatementSchema (AssignmentSchema _ (ExpressionSchema AddressOfSchema{}) _))
                scope
                typ
               ) = DeclarationSchema varSchema SkipSchema scope typ
convertForInit schema = schema


isInitialisedPointer :: AssemblySchema -> Bool
isInitialisedPointer (DeclarationSchema
                      _
                      (StatementSchema (AssignmentSchema _ (ExpressionSchema AddressOfSchema{}) _))
                      _
                      _
                     ) = True
isInitialisedPointer _ = False
