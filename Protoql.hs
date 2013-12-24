{-
Courtney Adams
Homework 6
Protoql.hs
Collaborators: Dan Monahan, Sam Choi
-}

module Protoql where

type Server = String
type Operation = String
type Constant = String

type InLabel = Integer
type OutLabel = Integer

data Query =
    Output (InLabel)
  | Query (InLabel, Server, Operation, OutLabel)
  | Input (Constant, OutLabel)
  deriving (Eq, Show)

data Program =
    Program [Query]
  deriving Eq    

instance Show Program where
  show (Program qs) =
    let fixOutput (Output i) = "Output (" ++ show i ++ ")" 
        fixOutput (q       ) = show q
    in foldr (++) "" [fixOutput q ++ ";\n" | q <- qs]


    
--eof