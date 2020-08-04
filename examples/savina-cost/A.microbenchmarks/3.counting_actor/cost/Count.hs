module Count where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

count :: CGT
count = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 1 $ \x -> do
    choices p [q] (Lbl 1, message p q (Var "\\tau_1") (CVar "c_1"))
                  (Lbl 2, message p q (Var "\\tau_1") (CVar "c_2") >> x)
    message p q (Var "\\tau_1") (CVar "c_1")
    message q p (Var "\\tau_2") (CVar "c_2")
    x

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

cCost :: Time
cCost = cost count

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes nints =
  Map.fromList
          [ ("\\tau_1", sz) -- in bytes
          , ("\\tau_2", sz)
          , ("c_1", 0)
            , ("c_2", 1.5e-10)  -- measured from counting actor
          ]
  where
    sz = fromInteger nints * 2

countSend :: Map (Role, Role) (Double -> Double)
countSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  ]
  where
    estSend = 8e-12

countRecv :: Map (Role, Role) (Double -> Double)
countRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-12

-- num iterations :
countTimes :: Double
countTimes = 1000000 * evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalDelta countSend countRecv (ppSizes i) cCost

countReal :: Double
countReal = 1.53e-4

main :: IO ()
main = do
  putStrLn "%% count"
  putStr "& "
  print countTimes
  putStr "& "
  print countReal
  putStr "& "
  print (abs (countTimes - countReal) / countReal)
