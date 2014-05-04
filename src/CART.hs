module CART where

import Data.List
import Numeric.LinearAlgebra hiding (find)
import Predictor
import System.Random
import Data.Random hiding (gamma)
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
                     alpha_grow :: Int,
                     alpha_prune :: Int,
                     gamma :: Double,
                     beta_grow :: Double,
                     beta_prune :: Double,
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

size (CART_Tree _ t) = tree_size t
tree_size (InteriorNode _ _ _ l r) = 1 + (tree_size l) + (tree_size r)
tree_size _ = 1

default_gamma = 0.9

new_CART_Params mu sigma al bet dim = CP
  {
    alpha_grow = al,
    alpha_prune = al,
    new_test = mk_new_test mu sigma,
    predictor_dim = dim,
    gamma = default_gamma,
    beta_grow = bet,
    beta_prune = bet,
    lambda = 1, -- I don't think I'd ever want to change this param
    num_tests = ceiling $ dim//3 -- Friedman says this is not right, and this should be considered a free param...
  }
  
type TestStats = (Vector Double, Double,  (Double, Int, Double), (Double, Int, Double))
                          
data NodeStats = NS 
                 {
                   node_size :: Int,
                   node_new_obs :: Int, 
                   oob_size :: Int, 
                   oob_error :: Double, 
                   estimate_mse :: Double,
                   node_prediction :: Double,
                   node_tests :: [TestStats]
                 }
                 deriving (Show)

default_CART_Tree dim = new_CART_Tree (fromList $ replicate dim 0) (fromList $ replicate dim 3) 50 0.25 dim

new_CART_Tree mu sigma alpha_grow beta_grow dim = do
  g <- newStdGen
  let ps = new_CART_Params mu sigma alpha_grow beta_grow dim
      (_,thresh) = new_tests ps g
  return $ CART_Tree ps $ createNode thresh (0,0,0)


data CART_Node = InteriorNode NodeStats (Vector Double) Double CART_Node CART_Node
               | LeafNode NodeStats
                 deriving (Show)
                 
                 
data CART_Tree = CART_Tree CART_Params CART_Node
               deriving (Show)
                 
instance Predictor CART_Node where
  predict v (InteriorNode s i x l r)
    |  (v`dot`i) < x = predict v l
    | otherwise  = predict v r
  predict _ (LeafNode stats) = node_prediction stats
    
instance Predictor CART_Tree where
  predict v (CART_Tree _ t) = predict v t

instance Learner CART_Tree where
  learn x y t@(CART_Tree p n) = do
    k <- getStdRandom $ sampleState (poisson $ lambda p) :: IO Int
    if k > 0 then foldM (\tr _ ->  observe (x,y) tr) t [1..k] else return t -- do
--      g <- newStdGen
--      return $ CART_Tree p $ observe_oob g p (x,y) n
  
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
observe_node g ps xy@(v,_) (InteriorNode s i x l r) 
  | (v`dot`i) < x = let l' = observe_node g ps xy l in InteriorNode (updateStats ps xy s) i x l' r
  | otherwise  = let r' = observe_node g ps xy r in InteriorNode (updateStats ps xy s) i x l r'
observe_node g ps xy (LeafNode stats) = let stats' = updateStats ps xy stats
                                        in if (alpha_grow ps) <= (node_new_obs stats')
                                           then try_split g ps stats'
                                           else LeafNode stats'

observe_oob :: StdGen -> CART_Params -> DataPoint -> CART_Node -> CART_Node
observe_oob g ps xy@(x,y) (InteriorNode s i tr l r) = case 
  if x`dot`i < tr
  then InteriorNode (updateOOB xy ps s) i tr (observe_oob g ps xy l) r
  else InteriorNode (updateOOB xy ps s) i tr l $ observe_oob g ps xy r of
    (InteriorNode s _ _ (LeafNode s1) (LeafNode s2)) 
      | should_prune ps s s1 s2 -> LeafNode $ s { node_tests = fst $ new_tests ps g }
    new_t -> new_t                 
observe_oob _ ps xy (LeafNode stats) = LeafNode $ updateOOB xy ps stats 
                                                                   
should_prune ps intstats leftstats rightstats 
  = (oob_error leftstats) * f + (oob_error rightstats) * (1-f) - (oob_error intstats) > (beta_prune ps)
   && (oob_size leftstats) + (oob_size rightstats) >= (alpha_prune ps)
  where f = (oob_size leftstats)//(oob_size leftstats + oob_size leftstats)
              
updateOOB (x,y) ps stats = stats {
  oob_error = ((oob_error stats) * f + (1-f) * ((y-(node_prediction stats))^2)),
  oob_size = 1 + oob_size stats
  }
  where f = (max 1 $ (oob_size stats)//(alpha_prune ps)) * (gamma ps) 


try_split :: StdGen -> CART_Params -> NodeStats -> CART_Node
try_split g ps stats = case flip find (node_tests stats) $ 
                            \(_,_,(_,lsz,lmse), (_,rsz,rmse)) 
                            -> (estimate_mse stats) - (lsz//(lsz+rsz)) * lmse - (rsz//(lsz+rsz)) * rmse > (beta_grow ps)
                       of
                         (Just (split, thresh, lstats, rstats)) 
                           -> let (t1,t2) = new_tests ps g 
                              in InteriorNode (stats { node_tests = [] }) split thresh (createNode t1 lstats) (createNode t2 rstats)
                         Nothing -> LeafNode stats

createNode tests (mu,size,mse) = LeafNode (NS { 
                                              node_size = size, 
                                              oob_size = 0,
                                              node_new_obs = 0,
                                              node_prediction = mu,
                                              node_tests = tests,
                                              oob_error = 0,
                                              estimate_mse = mse
                                           })

avg_update :: Int -> Double -> Double -> Double
avg_update n cur new = (cur * (fromIntegral n) + new) / (fromIntegral $ n+1)

updateStats ps xy@(x,y) stats = stats'
  where stats' = stats
          { 
            node_size = 1 + node_size stats,
            node_new_obs = 1 + node_new_obs stats,
            node_prediction = d',
            estimate_mse = avg_update (node_size stats) (estimate_mse stats) $ (y-d')^2,
            node_tests = map (\(spl,thr, (lmu,lsz,lmse), (rmu,rsz,rmse))
                               -> if spl`dot`x < thr
                                  then let lmu' = avg_update lsz lmu y 
                                       in (spl,thr,(lmu',lsz+1,avg_update lsz lmse $ (y-lmu')^2),(rmu,rsz,rmse))
                                  else let rmu' = avg_update rsz rmu y 
                                       in (spl,thr,(lmu,lsz,lmse),(rmu',rsz+1,avg_update rsz rmse $ (y-rmu')^2)))
                         $ node_tests stats
          }
        d' = f * (node_prediction stats) + (1-f) * y
        f = (max 1 $ (node_size stats)//(alpha_grow ps)) * (gamma ps)