OnlineRandomForest
==================

Online Random Forest (for regression)

This is a simple prototype for an Online Random Forest machine learning algorithm.
The Driver file has an interactive and batch testing mode.  

umm... 

    cabal install hmatrix easyplot rvar random-fu

Hi ConCon,

Anyhow, the code is pretty fucked up right now because I keep changing my mind about things.  
Unfortunately, right now my approach has TONS of free parameters (meaning difficult for an end user, which is not the case for RF.  I hope to fix this).  

Heres an extremely breif tutorial.  

There is a Driver.hs file which allows you to try it out on a fake example with a bunch of different parameter configurations and then graph the error rate as it goes.


Basically, baked_online_predictor takes 
1) a datafile
2) a list of (random forest,string) pairs to test on (the string is used for graphing)
3) the dimension of the problem (yes, this is known both from the data file and the forest, but whatever)
4) the bin size for reporting error rates, which you should just set to like 100

The datafile is a file where each line is one observation, which is a bunch of floats separated by spaces.
The first float is the thing being predicted, and the rest are the predictors.  
baked_online_predictor will see each line and try to predict on it before observing the datapoint.
One way of generating these things is with mixtureModel.
If you want one to get started with, type "mixtureStdBasis 10", which gives a ten dimensional problem, and saves it in the file "mix_10".  

The function some_tree_predictors is an example of using it, although there are too many forests used so the graph you get is a big convoluted. 
As I mentioned, there are a bunch of tuning params, the most interesting of which are alpha_grow, alpha_prune, beta_grow and beta_prune.
The alpha_grow is the minimum number of guys you need to make a new branch, the alpha_prune is the minimum number of guys you need on a branch to prune it.  beta_grow is the improvement you need to justify a new branch and beta_prune is the improvement you need to justify pruning it.
For offline trees you don't need these params since you can look at all the data at once, but online trees not only require this, but are somehow a bit sensitive to it.  

Anyway, if you want to play with it this might be enough to get you started.  If you want more details, just email me.  