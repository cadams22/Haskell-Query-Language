{-
Courtney Adams
Homework 6
Metaql.hs
Collaborators: Dan Monahan, Sam Choi
-}

module Metaql where

import Tree

data Exp =
    Plus Exp Exp
  | Conc Exp Exp
  | S String
  | N Integer
  deriving (Eq, Show)

data Stmt =
    Print Exp Stmt
  | End
  deriving (Eq, Show)
  
data Type =
    TyStr
  | TyInt
  | TyVoid
  deriving (Eq, Show)

instance Num Exp where
  fromInteger n = N n
  e1 + e2 = Plus e1 e2
  e1 * e2 = Conc e1 e2

tyExp :: Exp -> Maybe Type
tyExp (S string) = Just TyStr
tyExp (N integer) = Just TyInt
tyExp (Conc (e1) (e2)) = if ( tyExp(e1) == Just TyStr && tyExp(e2) == Just TyStr) then Just TyStr else Nothing
tyExp (Plus (e1) (e2)) = if ( tyExp(e1) == Just TyInt && tyExp(e2) == Just TyInt) then Just TyInt else Nothing

tyStmt :: Stmt -> Maybe Type
tyStmt (Print e s) = if tyExp(e) /= Nothing && tyStmt(s) /= Nothing then Just TyVoid else Nothing
tyStmt (End) = Just TyVoid

flat :: Exp -> [Exp]
flat (S string) = [S string]
flat (N integer) = [N integer]
flat (Plus e1 e2) = flat(e1)++flat(e2)
flat (Conc e1 e2) = flat(e1)++flat(e2)
{-insert (a) (Leaf b) = Node (Leaf a) (Leaf b)
-- look at all the subtrees, apply leaves to all, check smallest leaves, add
insert (a) (Node b c) = if leaves(b) < leaves(c) then Node (insert (a) (b)) c else Node (insert (a) (c)) b
-}

size :: Exp -> Int
size (S string) = 1
size (N integer) = 1
size (Plus e1 e2) = (size (e1)) + (size (e2))
size (Conc e1 e2) = (size (e1)) + (size (e2))

refactorInt :: Exp -> Exp
refactorInt (e) = if (tyExp(e) == Just TyInt) then fold (Plus) (foldr (insert) (Empty) (flat e) ) else e

refactorStr :: Exp -> Exp
refactorStr (e) = if (tyExp(e) == Just TyStr) then foldr1 (Conc) (flat e) else e

refactor :: Stmt -> Stmt 
refactor (End) = End
refactor (Print e s) = Print (refactorStr(refactorInt e)) (refactor s)
--eof