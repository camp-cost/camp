module TwoBuyer where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

-- global protocol TwoBuyer(role A, role B, role S) {
--     bookId(id: int) from A to S;
--     quoteA(x: int) from S to A; @"x >= 0"
--     quoteB(y: int) from S to B; @"x = y"
--     proposeA(a: int) from A to B; @"a >= 0 && a <= x"
--     choice at B {
--         ok(b: int) from B to A; @"b = y - a && b <= a"
--         buy() from A to S;
--     } or {
--         no() from B to A;
--         cancel() from A to S;
--     }
-- }
--

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

twoBuyer :: CGT
twoBuyer = gclose $ do
  a <- mkRole
  b <- mkRole
  s <- mkRole
  message a s (Var "\\tau_1") (CVar "c_1")
  message s a (Var "\\tau_1") (CVar "c_2")
  message s b (Var "\\tau_1") (CVar "c_3")
  message a b (Var "\\tau_1") (CVar "c_4")
  choices b [a]
    (Lbl 1, do
      message b a (Var "\\tau_1") (CVar "c_5")
      choice a s ((Lbl 1 ... pure ()) .| mempty)
    )

    (Lbl 2,
      choice a s ((Lbl 2 ... pure ()) .| mempty)
    )


tbCost :: Time
tbCost = cost twoBuyer

total :: Map Role Double -> Double
total = Map.foldl' max 0

sizes :: Map String Double
sizes = Map.fromList
  [ ("\\tau_1", 8)
  , ("c_1", 2.01)
  , ("c_2", 0)
  , ("c_3", 0)
  , ("c_4", 0.61)
  , ("c_5", 0.41)
  ]

tbSend :: Map (Role, Role) (Double -> Double)
tbSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  , ((Rol 1, Rol 2), const estSend)
  , ((Rol 2, Rol 0), const estSend)
  , ((Rol 0, Rol 2), const estSend)
  , ((Rol 2, Rol 0), const estSend)
  ]
  where
    estSend = 8e-12

tbRecv :: Map (Role, Role) (Double -> Double)
tbRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  , ((Rol 1, Rol 2), const estRecv)
  , ((Rol 2, Rol 0), const estRecv)
  , ((Rol 0, Rol 2), const estRecv)
  , ((Rol 2, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-12

tbTimes :: Double
tbTimes = total $ evalTime tbSend tbRecv sizes tbCost

main :: IO ()
main = do
  putStrLn "%% Two Buyer : "
  putStrLn $ "& " ++ show tbTimes
  putStrLn $ "& " ++ "3.0449380434782607"
  putStrLn $ "& " ++ show (abs (tbTimes - 3.0449380434782607) / 3.0449380434782607)
