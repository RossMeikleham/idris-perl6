module IRTS.CodegenPerl6(codegenPerl6) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char

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
doCodegen (n, SFun _ args i def) = cgFun n args def

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def 
    = "sub " ++ perlname n ++ "("
                  ++ showSep ", " (map (loc . fst) (zip [0..] args)) ++ ") {\n"
                  ++ cgBody doRet def ++ "\n}\n\n"
  where doRet :: String -> String -- Return the calculated expression
        doRet str = str ++ ";"

-- cgBody converts the SExp into a chunk of perl which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: (String -> String) -> SExp -> String
cgBody ret (SV (Glob n)) = ret $ perlname n ++ "()"
cgBody ret (SV (Loc i)) = ret $ loc i 
cgBody ret (SApp _ f args) = ret $ perlname f ++ "(" ++ 
                                   showSep "," (map cgVar args) ++ ")"
cgBody ret (SLet (Loc i) v sc)
   = cgBody (\x -> "my " ++ loc i ++ " = " ++ x ++ ";\n") v ++
     cgBody ret sc
cgBody ret (SUpdate n e)
   = cgBody ret e
cgBody ret (SProj e i)
   = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret (SCon _ t n args)
   = ret $ "array(" ++ showSep "," 
              (show t : (map cgVar args)) ++ ")"
cgBody ret (SCase _ e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "given (" ++ scr ++ ") {\n"
         ++ showSep "\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SChkCase e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "when (" ++ scr ++ ") {\n"
         ++ showSep "\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SConst c) = ret $ cgConst c
cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret SNothing = ret "0"
cgBody ret (SError x) = ret $ "error  " ++ show x
cgBody ret _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

cgAlt :: (String -> String) -> String -> SAlt -> String
cgAlt ret scr (SConstCase t exp)
   = "when (" ++ show t ++ ") {" ++ cgBody ret exp ++ "}"
cgAlt ret scr (SDefaultCase exp) = "default {" ++ cgBody ret exp ++ "}"
cgAlt ret scr (SConCase lv t n args exp)
   = "when (" ++ show t ++ ") {"
             ++ project 1 lv args ++ "\n" ++ cgBody ret exp ++ "}"
   where project i v [] = ""
         project i v (n : ns) = "my " ++ loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) (v + 1) ns

cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints, because PHP treats them as Strings...
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
cgOp LStrLen [x] = "chars(utf8_decode(" ++ x ++ "))"
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
cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



