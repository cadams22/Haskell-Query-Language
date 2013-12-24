{-
Courtney Adams
Final
Compile.hs
-}

module Compile where

import Protoql
import Metaql

compExp :: [InLabel] -> OutLabel -> Exp -> [Query]
compExp label outlabel (N integer) = [Input (show(integer), outlabel)]
compExp label outlabel (S string) = [Input (string, outlabel)]
compExp label outlabel (Plus e1 e2) = [Query(head label, "one.protoql.org", "Add", outlabel)] ++ (compExp (tail label) (head label) e1)++ (compExp (drop (size e1) (tail label)) (head label) e2)
compExp label outlabel (Conc e1 e2) = [Query(head label, "one.protoql.org", "Conc", outlabel)] ++ (compExp (tail label) (head label) e1)++(compExp (drop (size(e1)) (tail label)) (head label) e2)

compStmt :: [InLabel] -> Stmt -> [Query]
compStmt i (Print e s) = [Output (head i)] ++ (compExp (tail i) (head i) e ) ++ (compStmt (drop (size e) (tail i)) (s))

compile :: Stmt -> Maybe Program
compile stmt = if (tyStmt (stmt) == Just TyVoid) then Just (Program (compStmt [1..] (refactor stmt))) else Nothing

fromJust = maybe (error "Maybe.fromJust: Nothing") id

time :: Stmt -> Maybe Integer 
time End = Just 0 
time (Print e stmt) = Just ( max (calcTime e) (fromJust (time stmt) ))

calcTime :: Exp -> Integer 
calcTime e = if (tyExp e == Just TyInt) then toInteger(ceiling(logBase (fromIntegral 2) (fromIntegral (size (refactorInt e)) ))) else (toInteger(size(refactorStr e)) - 1)
