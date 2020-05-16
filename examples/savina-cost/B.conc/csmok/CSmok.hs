module CSmok where

import Control.Monad ( replicateM )
import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

csmok :: Int -> CGT
csmok nsmok = gclose $ do
  a <- mkRole
  s <- replicateM nsmok mkRole
  grec 1 $ \l ->
    choices a s
      (Lbl 1, do
        bcast a s (Var "\\tau_1") (CVar "c_1")
        recvAll s a (Var "\\tau_2") (CVar "c_2")
        l
      )
      (Lbl 2, bcast a s (Var "\\tau_3") (CVar "c_3"))
  where
    bcast p [] _ _ = pure ()
    bcast p (q:rs) t c = message p q t c >> bcast p rs t c

    recvAll [] p _ _ = pure ()
    recvAll (q:rs) p t c = message q p t c >> recvAll rs p t c

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

cCost :: Int -> Time
cCost = cost . csmok

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

csmokSend :: Map (Role, Role) (Double -> Double)
csmokSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  ]
  where
    estSend = 8e-12

csmokRecv :: Map (Role, Role) (Double -> Double)
csmokRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-12

-- Message sizes
-- num rounds = 1000
-- num smokers = 200
csmokTimes :: Double
csmokTimes = evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalTime csmokSend csmokRecv (ppSizes $ 200 `div` 4) (cCost 4)
