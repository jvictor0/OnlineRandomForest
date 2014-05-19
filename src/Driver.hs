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
             
random_forest_driver mu sigma alpha beta dim n = do
  for <- new_ensemble (new_CART_Tree $ new_CART_Params (fromList $ replicate dim mu) (fromList $ replicate dim sigma) alpha beta dim) n
  interactive_predictor dim for
    
baked_online_predictor fn ls dim bin_size = do
  pts <- fmap (map (map read . words) . lines) $ readFile fn :: IO [[Double]]
  alldats <- forM ls $ \(l,name) -> do 
    putStrLn $ "------ BEGINNING " ++ name ++ " ------------"
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
    return dats
  colors <- forM ls $ \_ -> do
    r <- randomRIO (0,255)
    g <- randomRIO (0,255)
    b <- randomRIO (0,255)
    return $ Color $ RGB r g b
  plot (PNG $ fn ++ "_error.png") $ map (\((_,name),(col,dats)) -> 
                                          Data2D [Title name, Style Lines, col] [] $ zip [0,fromIntegral bin_size..] dats) 
    $ zip ls $ zip colors alldats
  

some_tree_predictors :: FilePath -> Int -> IO Bool
some_tree_predictors fn dim = do
  fors <- fmap concat $ forM [25,50,75,100] $ \j -> do
    forM [0,10,20,30] $ \i -> do
      let ps = new_CART_Params (fromList $ replicate dim (1//dim)) (fromList $ replicate dim 3) j 0.2 dim
      let theps = ps { alpha_prune = i }
      for <- new_ensemble (new_CART_Tree theps) 100
      return (for,"grow = " ++ (show j) ++ " prune = " ++ (show i))
  baked_online_predictor fn fors dim 100


mixtureModel :: FilePath -> Int -> Int -> Int -> Double -> [([Double], Double, Double, Double)] -> IO ()
mixtureModel fp dim num_centres num_samples velocity meta_centres = do
  -- generate the centres
  centres <- fmap concat $ forM meta_centres $ \(mu, ymu, sigma, ysigma) -> do
    forM [1..num_centres] $ \_ -> do
      m <- mapM (\x -> getStdRandom $ sampleState (normal x 1)) mu
      v <- mapM (\x -> getStdRandom $ sampleState (normal 0 $ velocity/(fromIntegral dim))) mu
      ym <- getStdRandom $ sampleState $ normal ymu 1
      return (m,v, ym, sigma, ysigma)
  lines <- forM [1..num_samples] $ \i -> do
    (mu, v, ymu, sigma, ysigma) <- sampleRVar $ randomElement centres
    x <- mapM (\(mi,vi) -> getStdRandom $ sampleState $ normal (mi + (fromIntegral i) * vi) sigma) $ zip mu v
    y <- getStdRandom $ sampleState $ normal ymu ysigma
    return $ unwords $ map show $ y:x
  writeFile fp $ unlines lines
  
mixtureStdBasis n = mixtureModel ("mix_" ++ show n) n 10 2500 0.001 
                    $ map (\i -> ([if j == i then 1 else 0 | j <- [1..n]], 3.0 * (fromIntegral i), 0.2, 0.5)) [1..n]
