module ThreadRing where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

ring :: Int -> CGT
ring n = gclose $ do
  p <- mkRole
  grec 1 $ \x -> do
    ringIter p n
    x
  where
    ringIter p n = go p n
      where
        go q n =
          if n <= 0 then message q p (Var "\\tau_1") (CVar "c_1")
          else do
            r <- mkRole
            message q r (Var "\\tau_1") (CVar "c_1")
            go r (n-1)

ringCost :: Int -> Time
ringCost n = cost (ring n)

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes nints =
  Map.fromList
          [ ("\\tau_1", sz) -- in bytes
          , ("\\tau_2", sz)
          , ("c_1", 7e-10 + over)
          , ("c_2", 0)
          ]
  where
    over = fromInteger nints * 4.3500000000000008e-11
    sz = fromInteger nints * 2

thrSend :: Map (Role, Role) (Double -> Double)
thrSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  ]
  where
    estSend = 1.2e-10

thrRecv :: Map (Role, Role) (Double -> Double)
thrRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-11

-- Times the iterations
thrTimes :: Double
thrTimes = 100000 * evalIpc
  where
    evalIpc :: Double
    evalIpc = total $ evalDelta thrSend thrRecv (ppSizes 0) (ringCost 100) -- We run a ring of size 100 in Savina on a 4-core

thrReal :: Double
thrReal = 5.04e-3

main :: IO ()
main = do
  putStrLn "%% ring"
  putStr "& "
  print thrTimes
  putStr "& "
  print thrReal
  putStr "& "
  print (abs (thrTimes - thrReal) / thrReal)
