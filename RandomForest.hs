module RandomForest where

import Data.List
import Numeric.LinearAlgebra
import Predictor
import Utils
import CART

type RandomForest = EnsembleLearner CART_Tree

newtype EnsembleLearner learner = EL [learner]

instance (Predictor l) => Predictor (EnsembleLearner l) where
  predict v (EL ls) = (sum' (map (predict v) ls))/(fromIntegral $ length ls)
  
instance (Learner l) => Learner (EnsembleLearner l) where
  learn x y (EL ls) = fmap EL $ mapM (learn x y) ls
  
new_ensemble f n = fmap EL $ mapM (const f) [1..n]

