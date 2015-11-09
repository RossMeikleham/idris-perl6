module IRTS.CodegenPerl6(codegenPerl6) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char

import Control.Monad.State.Lazy

type CgState a = State Int a

-- Reset the variable number count 
resetVarCount :: Int -> CgState ()
resetVarCount i = do 
    put i

-- Increment the variable counter    
incVarCount :: CgState ()
incVarCount = do 
    i <- get
    put $ i + 1

codegenPerl6 :: CodeGenerator
codegenPerl6 ci = do let out = concatMap doCodegen (simpleDecls ci)
                     writeFile (outputFile ci) ("#!/usr/bin/perl\n" ++ 
                                                    uses ++ "\n" ++ 
                                                    helpers ++ "\n" ++
                                                    out ++ "\n" ++ 
                                                    start ++ "\n" ++ 
                                              "\n\n")

uses = concatMap (\m -> "use " ++ m ++ ";\n") []

start = perlname (sMN 0 "runMain") ++ "();"

helpers = errCode ++ "\n" ++ 
          doEcho ++ "\n" ++
          doRead ++ "\n" ++
          mkStr ++ "\n" ++
          doAppend ++ "\n"

errCode = "sub idris_error($str) {\n\tprint $str;\n\texit 0;\n}"
doEcho = "sub idris_writeStr($str) {\n\tprint $str\n}"
doRead = "sub idris_readStr {\n\tget;\n}"
doAppend = "sub idris_append($l, $r) {\n\t$l ~ $r;\n}"
mkStr = "sub mkStr($l) {\n\tmy @array = ($l);\n}"

perlname :: Name -> String
perlname n = "idris_" ++ concatMap perlchar (showCG n)
  where perlchar x | isAlpha x || isDigit x = [x]
                  | otherwise = "_" ++ show (fromEnum x) ++ "_"

var :: Name -> String
var n = "$" ++ perlname n

loc :: Int -> String
loc i = "$loc" ++ show i

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args i def) = evalState (cgFun n args def) 0 

cgFun :: Name -> [Name] -> SExp -> CgState String
cgFun n args def = do
    resetVarCount (length args)
    body <- cgBody doRet def
    return $ "sub " ++ perlname n ++ "("
                  ++ showSep ", " (map (loc . fst) (zip [0..] args)) ++ ") {\n"
                  ++ body ++ "\n}\n\n"
  where doRet :: String -> CgState String -- Return the calculated expression
        doRet str = return $ "return " ++ str ++ ";"

-- cgBody converts the SExp into a chunk of perl which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: (String -> CgState String) -> SExp -> CgState String
cgBody ret (SV (Glob n)) = ret $ perlname n ++ "()"
cgBody ret (SV (Loc i)) = ret $ loc i 
cgBody ret (SApp _ f args) = ret $ perlname f ++ "(" ++ 
                                   showSep "," (map cgVar args) ++ ")"
cgBody ret (SLet (Loc i) v sc) = do
   test <- get
   let b = show test ++ "\n"
   l <- cgBody (\x -> do 
                    s <- cgAssign i
                    return $ s ++ x ++ ";\n") v 
   r <- cgBody ret sc
   return $ l ++ r 

cgBody ret (SUpdate n e)
   = cgBody ret e
cgBody ret (SProj e i)
   = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret (SCon _ t n args)
   = ret $ "(" ++ showSep "," 
              (show t : (map cgVar args)) ++ ")"

cgBody ret (SCase _ e alts) = do
     let scrvar = cgVar e 
     let scr = if any conCase alts then scrvar ++ "[0]" else scrvar 
     alts <- mapM (cgAlt ret scrvar) alts 
     return $ "given (" ++ scr ++ ") {\n"
            ++ showSep "\n" alts ++ "\n}\n"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False

cgBody ret (SChkCase e alts) = do
       let scrvar = cgVar e 
       let scr = if any conCase alts then scrvar ++ "[0]" else scrvar 
       alts <- mapM (cgAlt ret scrvar) alts
       return $ "given (" ++ scr ++ ") {\n"
            ++ showSep "\n" alts ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False

cgBody ret (SConst c) = ret $ cgConst c
cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret SNothing = ret "0"
cgBody ret (SError x) = ret $ "idris_error  " ++ show x
cgBody ret _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

cgAlt :: (String -> CgState String) -> String -> SAlt -> CgState String
cgAlt ret scr (SConstCase t exp) = do
    oldState <- get
    body <- cgBody ret exp
    put oldState
    return $ "when (" ++ show t ++ ") {" ++ body ++ "}"

cgAlt ret scr (SDefaultCase exp) = do 
    body <- cgBody ret exp
    return $ "default {" ++ body ++ "}"

cgAlt ret scr (SConCase lv t n args exp) = do
    oldState <- get
    proj <- project 1 lv args
    body <- cgBody ret exp
    put oldState
    return $ "when (" ++ show t ++ ") {"
                ++ proj ++ "\n" ++ body ++ "}"
                
   where 
         project i v [] = return $ ""
         project i v (n : ns) = do
            assign <- cgAssign v
            proj <- project (i + 1) (v + 1) ns
            return $ assign ++ scr ++ "[" ++ show i ++ "]; " ++ proj


cgAssign :: Int -> CgState String
cgAssign v = do
    nVars <- get
    -- If var hasn't yet been initialised
    if v >= nVars 
        then do
            incVarCount 
            return $ "my " ++ loc v ++ " = "
        else do
            return $ loc v ++ " = "


cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints.
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r] 
     = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r] 
     = "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r] 
     = "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r] 
     = "(" ++ l ++ " == " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r] 
     = "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r] 
     = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r] 
     = "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r] 
     = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp LStrRev [x] = "flip(" ++ x ++ ")"
cgOp LStrLen [x] = "chars(" ++ x ++ ")"
cgOp LStrHead [x] = "ord(" ++ x ++ "[0])"
cgOp LStrIndex [x, y] = "ord(" ++ x ++ "[" ++ y ++ "])"
cgOp LStrTail [x] = "substr(" ++ x ++ ", 1)"

cgOp (LIntStr _) [x] = "\"" ++ x ++ "\""
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp LReadStr [_] = "idris_readStr()"
cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp LStrCons [l,r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgOp (LStrInt _) [x] = x
cgOp op exps = "idris_error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



