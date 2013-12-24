module A6Tests where

import Tree
import Metaql
import Protoql
import Compile

allTests = [
  show $ failed $ leavesTests,
  show $ failed $ insertTests,
  show $ failed $ foldTests,
  show $ failed $ tyTests,
  show $ failed $ flatTests,
  show $ failed $ sizeTests,
  show $ failed $ refactorTests
  ]

-- To get the failures for an individual test, query that
-- test using "failed", e.g.:
-- 
-- *> failed evalTests
-- *> failed execTests

failed :: Eq a => [(Integer, a, a)] -> [(Integer, a, a)]
failed tests = [(n, x, y) | (n, x, y) <- tests, x /= y]

leavesTests = [
  (1, leaves (Leaf True), 1),
  (2, leaves (Node (Node (Leaf 4) (Leaf 5)) (Leaf 7)), 3),
  (3, leaves (Node (Node (Leaf ["C"]) (Node (Node (Leaf []) (Leaf ["B"])) (Leaf []))) (Leaf ["A"])), 5),
  (4, leaves (Node (Node (Leaf (Node (Leaf 4) (Leaf 5))) (Leaf (Node (Leaf 4) (Leaf 5)))) (Leaf (Node (Leaf 4) (Leaf 5)))), 3)
  ]

insertTests = [
  (1, insert () Empty, Leaf ()),
  (2, insert () (insert () (Node (Leaf ()) (Leaf ()))), Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))),
  (3, insert () (insert () Empty), Node (Leaf ()) (Leaf ())),
  (4, insert () (insert () (insert () (insert () (insert () (insert () (insert () (insert () Empty))))))), Node (Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))) (Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))))
  ]
  
foldTests = [
  (1, fold (+) (Node (Node (Leaf 4) (Leaf 5)) (Leaf 7)), 16),
  (2, fold max (Node (Node (Leaf 4) (Leaf 5)) (Leaf 7)), 7),
  (3, fold (*) (Leaf 123), 123),
  (4, fold (\x y -> x) (Node (Node (Leaf 4) (Leaf 5)) (Leaf 7)), 4),
  (5, fold (\x y -> y) (Node (Node (Leaf 4) (Leaf 5)) (Leaf 7)), 7),
  (6, fold (+) (fold (\x y -> x) (Node (Node (Leaf (Node (Leaf 4) (Leaf 5))) (Leaf (Node (Leaf 4) (Leaf 5)))) (Leaf (Node (Leaf 4) (Leaf 5))))), 9)
  ]

tyTests = [
  (1, tyExp (1 + 2 + 3), Just TyInt),
  (2, tyExp (1 * 2 * 3), Nothing),
  (3, tyExp (S "BC" * 2 + S "A"), Nothing),
  (4, tyExp (S "BC" * S "D" * S "E" * S "A"), Just TyStr),
  (5, tyExp (S "BC" + S "D" + S "E" + S "A"), Nothing),
  (6, tyStmt (Print (S "BC" + S "D" + S "E" + S "A") $ End), Nothing),
  (7, tyStmt (Print (1 + 2 + 3) $ Print (S "BC" * S "D" * S "E" * S "A") $ End), Just TyVoid),
  (8, tyStmt (Print (1 + 2 + 3) $ Print (S "A" + 1) $ End), Nothing),
  (9, tyStmt (Print (1 + S "B") $ Print (2 + 1) $ End), Nothing),
  (10, tyStmt (Print (1 + 2) $ Print (S "C") $ Print (4 + 5) $ Print (S "F" * S "G") $ End), Just TyVoid)
  ]

flatTests = [
  (1, flat (1 + 2 + 3), [1,2,3]),
  (2, flat (S "A" * S "BC" * S "D" * S "E" * S "F"), [S x | x <- ["A","BC","D","E","F"]]),
  (3, flat (1 + S "A" + S "B"), [N 1, S "A", S "B"])
  ]

sizeTests = [
  (1, size (1 + 2 + 3), 3),
  (2, size (S "A" * S "BC" * S "D" * S "E" * S "F"), 5),
  (3, size (1 + S "A" + S "B"), 3)
  ]

refactorTests = [
  (1, Print (refactorInt (S "A" * S "B" * S "C")) End, Print (refactorInt (S "A" * S "B" * S "C")) End),
  (2, Print (refactorInt (foldr (+) 0 [1,2,3,4,5,6,7])) End, Print (Plus (Plus (Plus (N 1) (N 5)) (Plus (N 3) (N 7))) (Plus (Plus (N 2) (N 6)) (Plus (N 4) (N 0)))) End),
  (3, Print (refactorStr (foldl Conc (S "A") [S "B", S "C", S "D"])) End, Print (Conc (S "A") (Conc (S "B") (Conc (S "C") (S "D")))) End),
  (4, Print (refactorStr (((S "A" * S "B") * (S "C" * S "D")) * (S "E" * S "F") * (S "G" * S "H"))) End, Print (Conc (S "A") (Conc (S "B") (Conc (S "C") (Conc (S "D") (Conc (S "E") (Conc (S "F") (Conc (S "G") (S "H")))))))) End),
  (5, refactor (Print (foldr (+) 0 [1,2,3,4,5,6,7]) $ Print (foldl Conc (S "A") [S "B", S "C", S "D"]) $ End), Print (Plus (Plus (Plus (N 1) (N 5)) (Plus (N 3) (N 7))) (Plus (Plus (N 2) (N 6)) (Plus (N 4) (N 0)))) (Print (Conc (S "A") (Conc (S "B") (Conc (S "C") (S "D")))) End))
  ]
  
--eof