module PingPong where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

pingpong :: CGT
pingpong = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 1 $ \x -> do
    message p q (Var "\\tau_1") (CVar "c_1")
    message q p (Var "\\tau_2") (CVar "c_2")
    x

ppCost :: Time
ppCost = cost pingpong

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes nints =
  Map.fromList
          [ ("\\tau_1", sz) -- in bytes
          , ("\\tau_2", sz)
          , ("c_1", 7e-10) -- measured from pingpong.PingPongAkkaActorSeqCost
          , ("c_2", 0)
          ]
  where
    sz = fromInteger nints * 2

pingpongSend :: Map (Role, Role) (Double -> Double)
pingpongSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  ]
  where
    estSend = 1.2e-10

pingpongRecv :: Map (Role, Role) (Double -> Double)
pingpongRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-11

-- Message sizes
-- num pings = 40000
pingpongTimes :: Double
pingpongTimes = 40000 * evalIpc
  where
    evalIpc :: Double
    evalIpc = total $ evalDelta pingpongSend pingpongRecv (ppSizes 8) ppCost

ppReal :: Double
ppReal = 3.99e-5

main :: IO ()
main = do
  putStrLn "%% pp-akka"
  putStr "& "
  print pingpongTimes
  putStr "& "
  print ppReal
  putStr "& "
  print (abs (pingpongTimes - ppReal) / ppReal)
