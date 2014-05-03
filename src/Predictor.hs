module Predictor where

import Numeric.LinearAlgebra


class Predictor p where
  predict :: Vector Double -> p -> Double
       
class (Predictor p) => Learner p where
  learn :: Vector Double -> Double -> p -> IO p