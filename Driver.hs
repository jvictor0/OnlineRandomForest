module Driver where

import RandomForest
import Predictor
import Numeric.LinearAlgebra
import Utils
import Text.Read
import CART
import Data.IORef
import Control.Monad
import Control.Exception
import Data.Random
import System.Random
import Data.RVar
import Graphics.EasyPlot
import Data.List

commands = ["i","p"]

interactive_predictor :: Learner l => Int -> l -> IO ()
interactive_predictor dim l = do 
  line <- fmap words getLine
  if null line || ((head line) `notElem` commands)
    then do
    putStrLn "i <x1> ... <xp> <y> for insertion\np <x1> ... <xp> for prediction"
    interactive_predictor dim l
    else if head line == "i"
         then case mapM readMaybe $ tail line of
           (Just doubles) 
             | length doubles == dim + 1 -> do
             l' <- learn (fromList $ init doubles) (last doubles) l
             putStrLn "observation learned"
             interactive_predictor dim l'
           _ -> do
             putStrLn "cannot parse inputs"
             interactive_predictor dim l
         else case mapM readMaybe $ tail line of
           (Just doubles) 
             | length doubles == dim -> do
             let p = predict (fromList $ doubles) l
             putStrLn $ "y_hat = " ++ (show p)
             interactive_predictor dim l
           _ -> do
             putStrLn "cannot parse inputs"
             interactive_predictor dim l
             
random_forest_driver mu sigma alpha beta lambda dim n = do
  print dim
  for <- new_ensemble (new_CART_Tree (fromList $ replicate dim mu) (fromList $ replicate dim sigma) alpha beta lambda dim) n
  interactive_predictor dim for
    
baked_online_predictor fn l dim bin_size = do
  pts <- fmap (map (map read . words) . lines) $ readFile fn :: IO [[Double]]
  lref <- newIORef l
  me <- forM (zip [1..] pts) $ \(i,(y:xs)) -> do
    let x = fromList xs
    pred <- fmap (predict x) $ readIORef lref
    when (i`mod`bin_size == 0) $ do
      print i
      (EL ls) <- readIORef lref
      print $ map size ls
    evaluate pred
--    putStrLn $ "predicted " ++ (show pred) ++ " from " ++ (show xs)
--    putStrLn $ "  y = " ++ (show y)
--    putStrLn $ "  se = " ++ (show $ (y-pred)^2)
    newl <- (readIORef lref) >>= (learn x y)
    evaluate newl
    writeIORef lref newl
    return $ (y - pred)^2
  let dats = map ((/(fromIntegral bin_size)) . sum' .  map snd) $ groupBy (\(i,_) (j,_) -> i`div`bin_size  == j`div`bin_size) $ zip [0..] me
  plot (PNG $ fn ++ "_error.png") [Data2D [Title "Mean Squared Error vs Observation",Color Red,Style Points]
                                   [] (zip (map fromIntegral [0,bin_size..]) dats)]  


default_tree_predictor :: FilePath -> Int -> IO Bool
default_tree_predictor fn dim = do
  for <- new_ensemble (new_CART_Tree (fromList $ replicate dim (1//dim)) (fromList $ replicate dim 2) 50 0.2 1.0 dim) 100
  baked_online_predictor fn for dim 100


mixtureModel :: FilePath -> Int -> Int -> Int -> [([Double], Double, Double, Double)] -> IO ()
mixtureModel fp dim num_centres num_samples meta_centres = do
  -- generate the centres
  centres <- fmap concat $ forM meta_centres $ \(mu, ymu, sigma, ysigma) -> do
    forM [1..num_centres] $ \_ -> do
      m <- mapM (\x -> getStdRandom $ sampleState (normal x 1)) mu
      ym <- getStdRandom $ sampleState $ normal ymu 1
      return (m,ym, sigma, ysigma)
  lines <- forM [1..num_samples] $ \_ -> do
    (mu, ymu, sigma, ysigma) <- sampleRVar $ randomElement centres
    x <- mapM (\mi -> getStdRandom $ sampleState $ normal mi sigma) mu
    y <- getStdRandom $ sampleState $ normal ymu ysigma
    return $ unwords $ map show $ y:x
  writeFile fp $ unlines lines
  
mixtureStdBasis n = mixtureModel ("mix_" ++ show n) n 10 10000
                    $ map (\i -> ([if j == i then 1 else 0 | j <- [1..n]], 3.0 * (fromIntegral i), 0.2, 0.5)) [1..n]
