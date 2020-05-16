module DPhil where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

-- Num philosophers run this global type in parallel
dphil :: CGT
dphil = gclose $ do
  a <- mkRole -- arbitrator
  ph <- mkRole
  grec 1 $ \l ->
    choices ph [a]
      (Lbl 1,
         grec 1 $ \m -> do
           message ph a (Var "\\tau_1") (CVar "c_1")
           choices a [ph]
             (Lbl 2, do
              message a ph (Var "\\tau_2") (CVar "c_2")
              l
             )
             (Lbl 3, do
              message a ph (Var "\\tau_2") (CVar "c_2")
              m
             )
      )
      (Lbl 2, message ph a (Var "\\tau_3") (CVar "c_3"))

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

cCost :: Time
cCost = cost dphil

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes nints =
  Map.fromList
          [ ("\\tau_1", 1) -- in bytes
          , ("\\tau_2", 1)
          , ("c_1", 8e-8)
            , ("c_2", sz * 1.23e-10)  -- cost of arbitrator depends on num philosophers
          ]
  where
    sz = fromInteger nints

dphilSend :: Map (Role, Role) (Double -> Double)
dphilSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  ]
  where
    estSend = 8e-12

dphilRecv :: Map (Role, Role) (Double -> Double)
dphilRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-12

-- Message sizes
-- num eating rounds = 1000
-- num philosophers = 1000
dphilTimes :: Double
dphilTimes = evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalTime dphilSend dphilRecv (ppSizes 1000) cCost
