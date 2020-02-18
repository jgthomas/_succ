{-|
Module       : Generator
Description  : Produces assembly code

Generates the x86-64 assembly code for a particular abstract syntax tree.
-}
module Generator (generate) where


import           Control.Monad       (unless)
import           Control.Monad.Extra (concatMapM)
import           Data.Maybe          (fromMaybe)

import qualified ASM
import           AST                 (ArrayNode (..), Tree (..))
import           Error               (CompilerError (FatalError),
                                      FatalError (GeneratorBug))
import           GenState            (GenState, runGenState, throwError)
import qualified GenState            (getState, startState)
import           GenTokens           (Scope (..))
import           Operator            (BinaryOp (..), Operator (..))
import           SymbolTable         (SymTab)
import qualified SymTab


-- | Generate x86-64 asm from AST
generate :: Tree -> Either CompilerError (String, SymTab)
generate ast = runGenState genWithState ast GenState.startState


genWithState :: Tree -> GenState (String, SymTab)
genWithState ast = do
        asm    <- genASM ast
        symTab <- GenState.getState
        pure (asm, symTab)


genASM :: Tree -> GenState String

genASM (ProgramNode topLevelItems) = do
        text <- concatMapM genASM topLevelItems
        bss  <- concatMap ASM.uninitializedGlobal <$> SymTab.getUndefined
        dat  <- ASM.outputInit . concat <$> SymTab.getAllForInit
        pure $ dat ++ bss ++ text

genASM node@(FunctionNode _ _ _ Nothing _) = do
        declareFunction node
        pure ASM.noOutput
genASM node@(FunctionNode _ name _ (Just stmts) _) = do
        declareFunction node
        SymTab.initFunction name
        statements <- concatMapM genASM stmts
        SymTab.closeFunction
        SymTab.defineFunction name
        if hasReturn stmts || name /= "main"
           then pure $ ASM.function name statements
           else pure $ ASM.mainNoReturn name statements

genASM (ParamNode typ (VarNode name _) _) = do
        SymTab.addParameter name typ
        pure ASM.noOutput
genASM node@ParamNode{} =
        throwError $ FatalError (GeneratorBug node)

genASM (FuncCallNode name args _) =
        ASM.functionCall name <$> processArgs args

genASM (ArgNode arg _) = genASM arg

genASM (CompoundStmtNode blockItems _) = do
        SymTab.initScope
        blockLines <- mapM genASM blockItems
        SymTab.closeScope
        pure . concat $ blockLines

genASM (ForLoopNode ini test iter block _) = do
        SymTab.initScope
        passLabel <- SymTab.labelNum
        failLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        SymTab.setBreak failLabel
        SymTab.setContinue contLabel
        inits <- genASM ini
        tests <- genASM test
        iters <- genASM iter
        body  <- genASM block
        SymTab.closeScope
        pure $ ASM.forLoop inits tests iters body passLabel failLabel contLabel

genASM (WhileNode test whileBlock _) = do
        loopLabel <- SymTab.labelNum
        tests     <- genASM test
        testLabel <- SymTab.labelNum
        body      <- genASM whileBlock
        SymTab.setContinue loopLabel
        SymTab.setBreak testLabel
        pure $ ASM.while tests body loopLabel testLabel

genASM (DoWhileNode block test _) = do
        loopLabel <- SymTab.labelNum
        contLabel <- SymTab.labelNum
        body      <- genASM block
        tests     <- genASM test
        testLabel <- SymTab.labelNum
        SymTab.setContinue contLabel
        SymTab.setBreak testLabel
        pure $ ASM.doWhile body tests loopLabel contLabel testLabel

genASM (IfNode test action possElse _) = do
        testExp <- genASM test
        ifAct   <- genASM action
        label   <- SymTab.labelNum
        case possElse of
             Nothing -> pure $ ASM.ifOnly testExp ifAct label
             Just e  -> do
                     elseAct <- genASM e
                     ASM.ifElse testExp ifAct label elseAct <$> SymTab.labelNum

genASM (PointerNode varNode@VarNode{} typ Nothing dat) =
        genASM (DeclarationNode varNode typ Nothing dat)
genASM node@(PointerNode varNode@(VarNode varName _) typ (Just a) dat) = do
        pointerASM <- genASM (DeclarationNode varNode typ Nothing dat)
        value      <- genASM a
        (offset, _, globLab) <- SymTab.getVariables varName
        case (offset, globLab) of
             (Just off, _) -> pure $ ASM.varAddressStore (pointerASM ++ value) off
             (_, Just _)   -> pure $ pointerASM ++ value
             _             -> throwError $ FatalError (GeneratorBug node)
genASM node@PointerNode{} = throwError $ FatalError (GeneratorBug node)

genASM node@DeclarationNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> declareGlobal node
             Local  -> declareLocal node

genASM node@AssignmentNode{} = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> defineGlobal node
             Local  -> defineLocal node

genASM (AssignDereferenceNode derefNode@(DereferenceNode varName _) value op _) = do
        assign <- buildAssignmentASM derefNode value op
        (offset, argPos, globLab) <- SymTab.getVariables varName
        pure $ ASM.derefStore assign offset argPos globLab
genASM node@AssignDereferenceNode{} = throwError $ FatalError (GeneratorBug node)

genASM (ExprStmtNode expression _) = genASM expression

genASM (ContinueNode _) = ASM.setGotoPoint . fromMaybe (-1) <$> SymTab.getContinue

genASM (BreakNode _) = ASM.setGotoPoint . fromMaybe (-1) <$> SymTab.getBreak

genASM (ReturnNode tree _) = ASM.returnValue <$> genASM tree

genASM (TernaryNode cond pass fails _) = do
        testExp  <- genASM cond
        true     <- genASM pass
        false    <- genASM fails
        trueLab  <- SymTab.labelNum
        ASM.ternary testExp true false trueLab <$> SymTab.labelNum

genASM node@(BinaryNode _ (ConstantNode n _) (ShiftOp _) _) =
        processBinaryNode node (show n)
genASM node@(BinaryNode _ right _ _) = do
        rgt <- genASM right
        processBinaryNode node rgt

genASM (UnaryNode node@(VarNode a _) op _) = do
        unaryASM      <- genASM node
        (off, _, lab) <- SymTab.getVariables a
        pure $ ASM.unary unaryASM op off lab
genASM (UnaryNode tree  op _) = do
        unode <- genASM tree
        pure $ ASM.unary unode op Nothing Nothing

genASM node@(ArrayNode ArrayDeclareNode{}) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> declareGlobalArray node
             Local  -> declareLocalArray node

genASM (ArrayNode (ArrayItemsNode var items _)) = processArrayElements var items

genASM (ArrayNode (ArraySingleItemNode item _)) = genASM item

genASM (ArrayNode (ArrayItemAccess pos (VarNode name _) _)) = do
        (offset, argPos, globLab) <- SymTab.getVariables name
        let arrPosOffset = (+) (pos * SymTab.memOffset) <$> offset
        pure $ ASM.loadVariable arrPosOffset argPos globLab
genASM node@(ArrayNode ArrayItemAccess{}) = throwError $ FatalError (GeneratorBug node)

genASM (ArrayNode (ArrayAssignPosNode left right _ _)) = do
        value <- genASM right
        (++) value <$> genASM left

genASM (ArrayNode (ArrayItemAssign pos (VarNode name _) _)) = do
        (offset, _, globLab) <- SymTab.getVariables name
        let arrPosOffset = (+) (pos * SymTab.memOffset) <$> offset
        pure $ ASM.storeVariable arrPosOffset globLab
genASM node@(ArrayNode ArrayItemAssign{}) = throwError $ FatalError (GeneratorBug node)

genASM (VarNode name _) = do
        (offset, argPos, globLab) <- SymTab.getVariables name
        pure $ ASM.loadVariable offset argPos globLab

genASM (AddressOfNode name _) = do
        (offset, _, globLab) <- SymTab.getVariables name
        pure $ ASM.addressOf offset globLab

genASM (DereferenceNode name _) = do
        (offset, argPos, globLab) <- SymTab.getVariables name
        pure $ ASM.derefLoad offset argPos globLab

genASM (NullExprNode _) = pure ASM.noOutput

genASM (ConstantNode n _) = do
        currScope <- SymTab.getScope
        case currScope of
             Global -> pure . show $ n
             Local  -> pure . ASM.loadLiteral $ n


-- Arrays

processArrayElements :: Tree -> [Tree] -> GenState String
processArrayElements (VarNode name _) items = concatMapM processElement (zip items [0..])
        where processElement = processArrayElement name
processArrayElements tree _ = throwError $ FatalError (GeneratorBug tree)


processArrayElement :: String -> (Tree, Int) -> GenState String
processArrayElement name (item, pos) = do
        offset  <- SymTab.variableOffset name
        itemAsm <- genASM item
        case offset of
             Just off -> do
                     adjust <- SymTab.stackPointerValue
                     SymTab.incrementOffsetByN 1
                     pure $ ASM.assign itemAsm (pos * SymTab.memOffset + off) adjust
             Nothing  -> throwError $ FatalError (GeneratorBug item)


declareLocalArray :: Tree -> GenState String
declareLocalArray (ArrayNode (ArrayDeclareNode len var typ assign dat)) = do
        decAsm <- declareLocal (DeclarationNode var typ assign dat)
        case assign of
             Nothing -> do
                     SymTab.incrementOffsetByN (len - 1)
                     pure decAsm
             Just _  -> pure decAsm
declareLocalArray tree = throwError $ FatalError (GeneratorBug tree)


declareGlobalArray :: Tree -> GenState String
declareGlobalArray _ = undefined


-- Global variables

declareGlobal :: Tree -> GenState String
declareGlobal (DeclarationNode (VarNode name _) typ toAssign _) = do
        currLabel <- SymTab.globalLabel name
        case currLabel of
             Just _  -> genAssignment toAssign
             Nothing -> do
                     globLab <- SymTab.mkGlobLabel name
                     SymTab.declareGlobal name typ globLab
                     genAssignment toAssign
declareGlobal tree = throwError $ FatalError (GeneratorBug tree)


genAssignment :: Maybe Tree -> GenState String
genAssignment Nothing     = pure ASM.noOutput
genAssignment (Just tree) = genASM tree


defineGlobal :: Tree -> GenState String
defineGlobal node@(AssignmentNode (VarNode name _) _ _ _) = do
        label <- SymTab.globalLabel name
        SymTab.defineGlobal name
        defPrevDecGlob label node
defineGlobal tree = throwError $ FatalError (GeneratorBug tree)


defPrevDecGlob :: Maybe String -> Tree -> GenState String
defPrevDecGlob (Just label) (AssignmentNode _ node@ConstantNode{} _ _) = do
        value <- genASM node
        globalVarASM label value
defPrevDecGlob (Just label) (AssignmentNode _ node@AddressOfNode{} _ _) = do
        value   <- genASM node
        SymTab.storeForInit $ ASM.varAddressStoreGlobal value label
        pure $ ASM.uninitializedGlobal label
defPrevDecGlob _ (AssignmentNode _ valNode _ _) =
        throwError $ FatalError (GeneratorBug valNode)
defPrevDecGlob _ tree = throwError $ FatalError (GeneratorBug tree)


globalVarASM :: String -> String -> GenState String
globalVarASM lab "0" = pure $ ASM.uninitializedGlobal lab
globalVarASM lab val = pure $ ASM.initializedGlobal lab val


-- Local variables

declareLocal :: Tree -> GenState String
declareLocal (DeclarationNode (VarNode varName _) typ value _) = do
        offset <- SymTab.addVariable varName typ
        adjust <- SymTab.stackPointerValue
        case value of
             Just val -> genASM val
             Nothing  -> pure $ ASM.decNoAssign offset adjust
declareLocal tree = throwError $ FatalError (GeneratorBug tree)


defineLocal :: Tree -> GenState String
defineLocal node@(AssignmentNode varNode@(VarNode varName _) value op _) = do
        assign <- buildAssignmentASM varNode value op
        (offset, _, globLab) <- SymTab.getVariables varName
        case (offset, globLab) of
             (Just off, _) -> ASM.assign assign off <$> SymTab.stackPointerValue
             (_, Just lab) -> pure $ ASM.storeGlobal assign lab
             _ -> throwError $ FatalError (GeneratorBug node)
defineLocal tree = throwError $ FatalError (GeneratorBug tree)


-- Functions / function calls

declareFunction :: Tree -> GenState ()
declareFunction node@(FunctionNode _ funcName _ _ _) = do
        prevParamCount <- SymTab.decParamCount funcName
        case prevParamCount of
             Nothing -> declareNewFunction node
             Just _  -> declareRepeatFunction node
declareFunction tree = throwError $ FatalError (GeneratorBug tree)


declareNewFunction :: Tree -> GenState ()
declareNewFunction (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        processParameters funcName paramList
declareNewFunction tree = throwError $ FatalError (GeneratorBug tree)


declareRepeatFunction :: Tree -> GenState ()
declareRepeatFunction (FunctionNode typ funcName paramList _ _) = do
        SymTab.declareFunction typ funcName (length paramList)
        defined <- SymTab.checkFuncDefined funcName
        unless defined $
           do SymTab.delFuncState funcName
              processParameters funcName paramList
declareRepeatFunction tree = throwError $ FatalError (GeneratorBug tree)


processParameters :: String -> [Tree] -> GenState ()
processParameters name params = do
        SymTab.initFunction name
        mapM_ genASM params
        SymTab.closeFunction


hasReturn :: [Tree] -> Bool
hasReturn [] = False
hasReturn items =
        case last items of
             ReturnNode{} -> True
             _            -> False


processArgs :: [Tree] -> GenState String
processArgs args = concatMapM processArg (zip args [0..])


processArg :: (Tree, Int) -> GenState String
processArg (arg, pos) = do
        argASM <- genASM arg
        pure $ ASM.passArgument argASM pos


-- Variables

buildAssignmentASM :: Tree -> Tree -> Operator -> GenState String
buildAssignmentASM _ valTree Assignment = genASM valTree
buildAssignmentASM varTree@(DereferenceNode _ dat) valTree (BinaryOp binOp) =
        genASM (BinaryNode varTree valTree binOp dat)
buildAssignmentASM varTree@(VarNode _ dat) valTree (BinaryOp binOp) =
        genASM (BinaryNode varTree valTree binOp dat)
buildAssignmentASM varTree _ _ =
        throwError $ FatalError (GeneratorBug varTree)


-- Operators

processBinaryNode :: Tree -> String -> GenState String
processBinaryNode (BinaryNode left _ op _) rgt = do
        lab1 <- SymTab.labelNum
        lab2 <- SymTab.labelNum
        lft  <- genASM left
        pure $ ASM.binary lft rgt op lab1 lab2
processBinaryNode tree _ = throwError $ FatalError (GeneratorBug tree)
