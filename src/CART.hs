module CART where

import Data.List
import Numeric.LinearAlgebra hiding (find)
import Predictor
import System.Random
import Data.Random
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Dirichlet
import Utils
import Control.Monad.State
import Data.RVar
import Debug.Trace

type DataPoint = (Vector Double,Double)

data CART_Params = CP
                   {
                     alpha :: Int,
                     beta :: Double,
                     new_test :: StdGen -> ((Vector Double,Double),StdGen),
                     predictor_dim :: Int,
                     lambda :: Double,
                     num_tests :: Int
                   }

instance Show CART_Params where
  show _ = "CART_Params"

dirichlet_param dim = replicate dim 0.01

mk_new_test :: Vector Double -> Vector Double -> StdGen -> ((Vector Double, Double),StdGen)
mk_new_test mu sigma g = flip runState g $ do
  splitdir <- fmap fromList $ sampleRVar $ dirichlet $ dirichlet_param $ dim mu 
  threshvect <- fmap fromList $ mapM (\i -> sampleRVar $ normal (mu@>i) (sigma@>i)) [0..(dim mu)-1]
  return $ (splitdir,threshvect`dot`splitdir)

size (CART_Tree _ t) = node_size t
node_size (InteriorNode _ _ l r) = 1 + (node_size l) + (node_size r)
node_size _ = 1


new_CART_Params mu sigma al bet dim = CP
  {
    alpha = al,
    new_test = mk_new_test mu sigma,
    predictor_dim = dim,
    beta = bet,
    lambda = 1,
    num_tests = ceiling $ dim//3 -- Friedman says this is not right, and this should be considered a free param...
  }
  
type TestStats = (Vector Double, Double,  (Double, Int, Double), (Double, Int, Double))
                          
data LeafStats = LS 
                 {
                   leaf_size :: Int,
                   leaf_new_obs :: Int, 
                   estimate_mse :: Double,
                   leaf_prediction :: Double,
                   leaf_tests :: [TestStats]
                 }
                 deriving (Show)

default_CART_Tree dim = new_CART_Tree (fromList $ replicate dim 0) (fromList $ replicate dim 3) 50 0.25 dim

new_CART_Tree mu sigma alpha beta dim = do
  g <- newStdGen
  let ps = new_CART_Params mu sigma alpha beta dim
      (_,thresh) = new_tests ps g
  return $ CART_Tree ps $ createNode thresh (0,0,0)


data CART_Node = InteriorNode (Vector Double) Double CART_Node CART_Node
               | LeafNode LeafStats
                 deriving (Show)
                 
                 
data CART_Tree = CART_Tree CART_Params CART_Node
               deriving (Show)
                 
instance Predictor CART_Node where
  predict v (InteriorNode i x l r)
    |  (v`dot`i) < x = predict v l
    | otherwise  = predict v r
  predict _ (LeafNode stats) = leaf_prediction stats
    
instance Predictor CART_Tree where
  predict v (CART_Tree _ t) = predict v t

instance Learner CART_Tree where
  learn x y t@(CART_Tree p n) = do
    k <- getStdRandom $ sampleState (poisson $ lambda p) :: IO Int
    foldM (\tr _ ->  observe (x,y) tr) t [1..k]
  
new_tests :: CART_Params -> StdGen -> ([TestStats],[TestStats])
new_tests cps g = flip evalState g $ do
  t1 <- forM [1.. num_tests cps] $ \_ -> do
    g <- get
    let ((v,d),g') = new_test cps g
    put g'
    return (v,d,(0:: Double,0::Int,0::Double),(0::Double,0::Int,0::Double))
  t2 <- forM [1.. num_tests cps] $ \_ -> do
    g <- get
    let ((v,d),g') = new_test cps g
    put g'
    return (v,d,(0::Double,0::Int,0::Double),(0::Double,0::Int,0::Double))
  return (t1,t2)
  
  
observe :: DataPoint -> CART_Tree -> IO CART_Tree
observe dp (CART_Tree ps t) = do
  g <- newStdGen
  let new_t = observe_node g ps dp t
  return $ CART_Tree ps new_t

observe_node :: StdGen -> CART_Params -> DataPoint -> CART_Node -> CART_Node
observe_node g ps xy@(v,_) (InteriorNode i x l r) 
  | (v`dot`i) < x = let l' = observe_node g ps xy l in InteriorNode i x l' r
  | otherwise  = let r' = observe_node g ps xy r in InteriorNode i x l r'
observe_node g ps xy (LeafNode stats) = let stats' = updateStats xy stats
                                        in if (alpha ps) <= (leaf_new_obs stats')
                                           then try_split g ps stats'
                                           else LeafNode stats'

try_split :: StdGen -> CART_Params -> LeafStats -> CART_Node
try_split g ps stats = case flip find (leaf_tests stats) $ 
                            \(_,_,(_,lsz,lmse), (_,rsz,rmse)) 
                            -> (estimate_mse stats) - (lsz//(lsz+rsz)) * lmse - (rsz//(lsz+rsz)) * rmse > (beta ps)
                       of
                         (Just (split, thresh, lstats, rstats)) 
                           -> let (t1,t2) = new_tests ps g 
                              in InteriorNode split thresh (createNode t1 lstats) (createNode t2 rstats)
                         Nothing -> LeafNode stats

createNode tests (mu,size,mse) = LeafNode (LS { 
                                              leaf_size = size, 
                                              leaf_new_obs = 0,
                                              leaf_prediction = mu,
                                              leaf_tests = tests,
                                              estimate_mse = mse
                                           })

avg_update :: Int -> Double -> Double -> Double
avg_update n cur new = (cur * (fromIntegral n) + new) / (fromIntegral $ n+1)

updateStats xy@(x,y) stats = stats'
  where stats' = stats
          { 
            leaf_size = 1 + leaf_size stats,
            leaf_new_obs = 1 + leaf_new_obs stats,
            leaf_prediction = d',
            estimate_mse = ((estimate_mse stats) * (fromIntegral $ leaf_size stats) + (y-d')^2) / (fromIntegral $ leaf_size stats'),
            leaf_tests = map (\(spl,thr, (lmu,lsz,lmse), (rmu,rsz,rmse))
                               -> if spl`dot`x < thr
                                  then let lmu' = avg_update lsz lmu y 
                                       in (spl,thr,(lmu',lsz+1,avg_update lsz lmse $ (y-lmu')^2),(rmu,rsz,rmse))
                                  else let rmu' = avg_update rsz rmu y 
                                       in (spl,thr,(lmu,lsz,lmse),(rmu',rsz+1,avg_update rsz rmse $ (y-rmu')^2)))
                         $ leaf_tests stats
          }
        d' = ((leaf_prediction stats) * (fromIntegral $ leaf_size stats) + y) / (fromIntegral $ leaf_size stats')