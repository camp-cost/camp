module Barber where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

barber :: CGT
barber = gclose $ do
  barb <- mkRole
  sel <- mkRole
  cust <- mkRole
  room <- mkRole
  grec 1 $ \l -> do
    message sel room (Var "\\tau_1") (CVar "c_1")
    choices room [cust, sel]
      (Lbl 1, do
          message room cust (Var "\\tau_2") (CVar "c_2")
          message cust sel (Var "\\tau_3") (CVar "c_3")
          l)
      (Lbl 2, do
          message room cust (Var "\\tau_4") (CVar "c_4")
          message cust sel (Var "\\tau_5") (CVar "c_5"))
    message barb cust (Var "\\tau_6") (CVar "c_6")
    message cust sel (Var "\\tau_7") (CVar "c_7")
    message sel room (Var "\\tau_8") (CVar "c_8")
    message sel room (Var "\\tau_9") (CVar "c_9")

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

cCost :: Time
cCost = cost barber

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes nints =
  Map.fromList
          [ ("\\tau_1", 1) -- in bytes
          , ("\\tau_2", 1)
          , ("c_1", 0)
            , ("c_2", sz * 3.5e-11)  --
          ]
  where
    sz = fromInteger nints * 2

barberSend :: Map (Role, Role) (Double -> Double)
barberSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  ]
  where
    estSend = 8e-12

barberRecv :: Map (Role, Role) (Double -> Double)
barberRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-12

-- Message sizes
-- num haircuts = 5000
barberTimes :: Double
barberTimes = 5000 * evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalDelta barberSend barberRecv (ppSizes ahr) cCost
    ahr = 1000 -- busywait takes a random amount of time in implementation. we set it to a fixed parameter that we computed as an average execution time for AHR = 1000

barberReal :: Double
barberReal = 3.36e-4

main :: IO ()
main = do
  putStrLn "%% barb"
  putStr "& "
  print barberTimes
  putStr "& "
  print barberReal
  putStr "& "
  print (abs (barberTimes - barberReal) / barberReal)
