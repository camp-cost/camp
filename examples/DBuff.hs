module DBuff where

import Language.SessionTypes.Common
import Language.SessionTypes.Cost
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

dbuff :: CGT
dbuff = gclose $ do
  src <- mkRole
  snk <- mkRole
  serv <- mkRole
  send serv src (Var "r1")
  send serv src (Var "r2")
  grec 1 $ \l -> do
    recv src serv (Var "r1") (CVar "cr")
    message src serv (Var "s1") (CVar "cs")
    message snk serv (Var "t1") (CVar "ct")
    message serv snk (Var "u1") (CVar "cu")
    send serv src (Var "r1")

    recv src serv (Var "r2") (CVar "cr")
    message src serv (Var "s2") (CVar "cs")
    message snk serv (Var "t2") (CVar "ct")
    message serv snk (Var "u2") (CVar "cu")
    send serv src (Var "r2")
    l

dbuffU :: CGT
dbuffU = gclose $ do
  src <- mkRole
  snk <- mkRole
  serv <- mkRole
  grec 1 $ \l -> do
    message serv src (Var "r1") (CVar "cr")
    message src serv (Var "s1") (CVar "cs")
    message snk serv (Var "t1") (CVar "ct")
    message serv snk (Var "u1") (CVar "cu")

    message serv src (Var "r2") (CVar "cr")
    message src serv (Var "s2") (CVar "cs")
    message snk serv (Var "t2") (CVar "ct")
    message serv snk (Var "u2") (CVar "cu")
    l

choices :: Role -> [Role] -> (Label, GTM ()) -> (Label, GTM ()) -> GTM ()
choices _ [] _ _ = pure ()
choices a (b:cs) (ll, gl) (lr, gr)
  = choice a b ((ll ... choiceL ll cs gl) .| ((lr ... choiceL lr cs gr) .| mempty))
  where
    choiceL _l [] g = g
    choiceL l (c:ds) g = choice a c ((l ... choiceL l ds g) .| mempty)

cCost :: Time
cCost = cost dbuff

cCostU :: Time
cCostU = cost dbuff

total :: Map Role Double -> Double
total = Map.foldl' max 0

ppSizes :: Integer -> Map String Double
ppSizes _nints =
  Map.fromList
          [ ("\\tau_1", 1) -- in bytes
          , ("\\tau_2", 1)
          , ("cs", 0.051426 / 1000) -- measured for 1000 reps in Arch1
          , ("cr",  0.0755 / 1000)
          , ("cu",  0.001117 / 1000)
          ]

dbuffSend :: Map (Role, Role) (Double -> Double)
dbuffSend = Map.fromList
  [ ((Rol 0, Rol 1), const estSend)
  , ((Rol 1, Rol 0), const estSend)
  , ((Rol 1, Rol 2), const estSend)
  , ((Rol 2, Rol 0), const estSend)
  , ((Rol 0, Rol 2), const estSend)
  , ((Rol 2, Rol 0), const estSend)
  ]
  where
    estSend = 8e-12

dbuffRecv :: Map (Role, Role) (Double -> Double)
dbuffRecv = Map.fromList
  [ ((Rol 0, Rol 1), const estRecv)
  , ((Rol 1, Rol 0), const estRecv)
  , ((Rol 1, Rol 2), const estRecv)
  , ((Rol 2, Rol 0), const estRecv)
  , ((Rol 0, Rol 2), const estRecv)
  , ((Rol 2, Rol 0), const estRecv)
  ]
  where
    estRecv = 8e-12

-- Message sizes
-- num rounds = 1000
dbuffTimes :: Double
dbuffTimes = evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalTime dbuffSend dbuffRecv (ppSizes i) cCost

dbuffTimesU :: Double
dbuffTimesU = evalIpc 1
  where
    evalIpc :: Integer -> Double
    evalIpc i = total $ evalTime dbuffSend dbuffRecv (ppSizes i) cCostU

main :: IO ()
main = do
  putStrLn "%% DBuff Optimised: "
  putStrLn $ "& " ++ show (dbuffTimes * 1000)
  putStrLn $ "& " ++ "0.20544"
  putStrLn $ "& " ++ show (abs (dbuffTimes * 1000 - 0.20544) / 0.20544)

  putStrLn "\n\n%% DBuff Unoptimised: "
  putStrLn $ "& " ++ show (dbuffTimesU * 1000)
  putStrLn $ "& " ++ "0.236397"
  putStrLn $ "& " ++ show (abs (dbuffTimesU * 1000 - 0.236397) / 0.236397)

