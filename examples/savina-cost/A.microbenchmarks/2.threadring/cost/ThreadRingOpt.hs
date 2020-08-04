module ThreadRing where

import Control.Monad ( replicateM )
import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

sendAll :: Role -> [Role] -> GTM ()
sendAll p (q : qs@(r : _))
  = send q r (Var "\\tau") >> sendAll p qs
sendAll p [q] = send q p (Var "\\tau")
sendAll _ _ = pure ()

recvAllC :: String -> Role -> [Role] -> GTM ()
recvAllC c p (q : qs@(r : _))
  = recv q r (Var "\\tau") (CVar c) >> recvAllC c p qs
recvAllC c p [q] = recv q p (Var "\\tau") (CVar c)
recvAllC _ _ _ = pure ()

recvAll :: Role -> [Role] -> GTM ()
recvAll = recvAllC "c"


ringM :: [Role] -> GTM ()
ringM ps@(p : _) = grec 1 $ \x -> sendAll p ps >> recvAll p ps >> x
ringM [] = pure ()

mkRoles :: Int -> GTM [Role]
mkRoles i = replicateM i mkRole

ringP :: Int -> CGT
ringP i = gclose $ mkRoles i >>= ringM

ringCost :: Int -> Time
ringCost n = cost (ringP n)

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes nints =
  Map.fromList
          [ ("\\tau", 4) -- in bytes
          , ("c", 7e-10 + over)
          ]
  where
    over = fromInteger nints * 4.3500000000000008e-11

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

-- iterations :
thrTimes :: Double
thrTimes = 100000 * evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalDelta thrSend thrRecv (ppSizes 100) (ringCost 4) -- We run a ring of size 100 in Savina in a 4-core machine

thrReal :: Double
thrReal = 5.4e-4

main :: IO ()
main = do
  putStrLn "%% ring"
  putStr "& "
  print thrTimes
  putStr "& "
  print thrReal
  putStr "& "
  print (abs (thrTimes - thrReal) / thrReal)
