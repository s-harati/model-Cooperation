runs.df = data.frame(mu=numeric(),sigma=numeric(),gamma=numeric(),
                   n=numeric(),epsilon=numeric(),alpha=numeric(),
                   writeThresholds=numeric())
vec.alpha = c(.2 , .4 , .6 , .8 , 1)
vec.epsilon = c(.01 , .1 , .2)
vec.n = c(5 , 9 , 13)
vec.gamma = c(.1 , .5 , .9)
vec.sigma = c(.06 , .08)
vec.mu = c(.3 , .5 , .7)
row.counter = 1
for (mu in vec.mu) {
  for (sigma in vec.sigma) {
    for (gamma in vec.gamma) {
      for (n in vec.n) {
        for (epsilon in vec.epsilon) {
          for (alpha in vec.alpha) {
            if ((gamma==vec.gamma[1]) & (epsilon==vec.epsilon[1]) & (alpha==vec.alpha[1])) {
              writeThresholds = 1
            } else {
              writeThresholds = NA
            }
            runs.df[row.counter,]=c(mu,sigma,gamma,n,epsilon,alpha,writeThresholds)
            row.counter = row.counter + 1
          }
        }
      }
    }
  }
}
dir.out = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/analysis/"
write.csv(runs.df,paste(dir.out,"paramsPermutations.csv",sep=""))
