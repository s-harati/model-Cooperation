# figures of the RL paper

#################
#### Figures ####
#################
# Figure 3 - histogram of overall scores of episodes in reference and simulation datasets (15)
# 3a
hist(array.meanscores.ref, main="Histogram of reference scores",xlab="",xlim=c(-1,0))
# saved 400x400
# 3b
hist(array.meanscores.sim, main="Histogram of simulation scores",xlab="",xlim=c(-1,0))
# saved 400x400
# Figure 4 - histograms of scores of simulations (1 plot for each simulation) (16)
for (i in 1:dim(array.meanscores.sim)[3]) { # 1:6 algorithms
  vec.temp = numeric()
  vec.temp = as.vector(array.meanscores.sim[,,i])
  if (dimnames(array.meanscores.sim)[[3]][i]=="TD_DoubleExpectedSARSA") {name.temp = "TD Double Expected SARSA"} #4e
  if (dimnames(array.meanscores.sim)[[3]][i]=="TD_DoubleQLearning") {name.temp = "TD Double Q-Learning"} #4f
  if (dimnames(array.meanscores.sim)[[3]][i]=="TD_DoubleSARSA") {name.temp = "TD Double SARSA"} #4d
  if (dimnames(array.meanscores.sim)[[3]][i]=="TD_expectedSARSA") {name.temp = "TD Expected SARSA"} #4b
  if (dimnames(array.meanscores.sim)[[3]][i]=="TD_QLearning") {name.temp = "TD Q-Learning"} #4c
  if (dimnames(array.meanscores.sim)[[3]][i]=="TD_SARSA") {name.temp = "TD SARSA"} #4a
  hist(vec.temp,main=name.temp,xlab="",xlim=c(-1,0),ylim=c(0,30000))
}
# all saved 400x400
# Figure 5 - heatmaps of overall scores of simulations with and without respect to reference (17)
# 6a: lowest scores (red column) for mu=0.7 . highest scores (top one-third zone) for epsilon=0.01 
heatmap(mat.heatmap.sim.scores , main="Simulation scores heatmap",
        zlim=c(-1,0) , scale="none")
# 6b: basically same order as 6a, but with respect to reference we see that the difference between mu=0.7 and other mu's is not as substantial as suggested in 6a
heatmap(mat.heatmap.simNref.scores , main="Simulation vs. reference scores heatmap",
        zlim=c(-1,0) , scale="none")
# both saved 777x800
#
# Figure 6 - heatmap of ranks of simulations (18)
heatmap(mat.heatmap.sim.ranks , main="Simulation ranks heatmap" , 
        scale="none")
# saved 777x800
#
# Figure 7 - boxplots of time step scores in selected algorithms [REMOVED: and reference dataset] (19)
# mat.temp = numeric()
# for (i in 1:dim(array.timeSteps.ref)[2]) { # 1:17 timeSteps
#   vec.temp = as.vector(array.timeSteps.ref[,i,])
#   mat.temp = cbind(mat.temp , vec.temp)
# } # dim(mat.temp) = 40500   17
# # 7a PROBLEM! array.timeSteps.ref is MEAN of 2^17 runs for each entry
# boxplot(mat.temp,ylim=c(-1,0),xaxt="n",xlab="time",ylab="rewards",
#         main="Reference")
# axis(1,at=1:ncol(mat.temp),labels=1:ncol(mat.temp))
#
mat.temp = numeric()
for (i in 1:dim(array.timeSteps.ref)[2]) { # 1:17 timeSteps
  vec.temp = as.vector(array.timeSteps.sim[,i,,"TD_DoubleSARSA"])
  mat.temp = cbind(mat.temp , vec.temp)
} # dim(mat.temp) = 40500   17
# 7a
boxplot(mat.temp,ylim=c(-1,0),xaxt="n",xlab="time",ylab="rewards",
        main="Double SARSA")
axis(1,at=1:ncol(mat.temp),labels=1:ncol(mat.temp))
#
mat.temp = numeric()
for (i in 1:dim(array.timeSteps.ref)[2]) { # 1:17 timeSteps
  vec.temp = as.vector(array.timeSteps.sim[,i,,"TD_DoubleExpectedSARSA"])
  mat.temp = cbind(mat.temp , vec.temp)
} # dim(mat.temp) = 40500   17
# 7b
boxplot(mat.temp,ylim=c(-1,0),xaxt="n",xlab="time",ylab="rewards",
        main="Double Expected SARSA")
axis(1,at=1:ncol(mat.temp),labels=1:ncol(mat.temp))
# all saved 400x400

# Figure 8 - boxplots of overall scores in selected algorithms and reference dataset (20)
# 8a
boxplot(array.meanscores.ref,ylim=c(-1,0),main="Reference")
vec.temp = as.vector(array.meanscores.ref)
boxplot(vec.temp,ylim=c(-1,0),main="Reference")
#
vec.temp = as.vector(array.meanscores.sim[,,"TD_DoubleSARSA"])
# 8b
boxplot(vec.temp,ylim=c(-1,0),main="TD Double SARSA")
#
vec.temp = as.vector(array.meanscores.sim[,,"TD_DoubleExpectedSARSA"])
# 8c
boxplot(vec.temp,ylim=c(-1,0),main="TD Double Expected SARSA")
# all saved 300x400

# new ideas?
## histograms can show the effect of mu, helping identify 
## the scope in which solutions may be expected for the problem

## need to identify two different validation analyses:
### tree of all decisions, and random baseline
### why not base everything on the tree?
#### the problem was that both RL and ref scores were low
#### one wondered what is the use of the model
#### but then, is the big tree really a benchmark for comparison?
#### is it reasonable to say that the reference G picks a random 'chain of decisions'? 
#### or is it more reasonable for the reference G to make random decisions?
#### the latter is at each step

## last figure can be boxplot of only timestep 17,
## to show how episodes end.

#########################
# revisiting questions: #
#########################
folder.social = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/analysis/"

file.array.timeSteps = paste(folder.social,"arrayTimeSteps",sep="")
load(file.array.timeSteps) # array 810 x 17 x 6
file.array.sim = paste(folder.social,"arrayTimeStepsSim",sep="")
load(file.array.sim) # array 810 x 17 x 50 x 6
file.array.ref = paste(folder.social,"arrayTimeStepsRef",sep="")
load(file.array.ref) # array 810 x 17 x 50
file.params = paste(folder.social,"paramsPermutations.csv",sep="")
params = read.csv(file.params) # data.frame

vec.algorithms = dimnames(array.timeSteps.sim)[[4]]

unique.epsilon = unique(params$epsilon)
unique.alpha = unique(params$alpha)
unique.mu = unique(params$mu)
unique.sigma = unique(params$sigma)
unique.n = unique(params$n)
unique.gamma = unique(params$gamma)

# What is the decision space like? Under what conditions is emergence of cooperation more likely?
## Histograms of the 18 sets of conditions. Any difference?
# recall: the 18 indices:
#  1 mu30 sd06 n5 #  2 mu30 sd06 n9 #  3 mu30 sd06 n13
#  4 mu30 sd08 n5 #  5 mu30 sd08 n9 #  6 mu30 sd08 n13
#  7 mu50 sd06 n5 #  8 mu50 sd06 n9 #  9 mu50 sd06 n13
# 10 mu50 sd08 n5 # 11 mu50 sd08 n9 # 12 mu50 sd08 n13
# 13 mu70 sd06 n5 # 14 mu70 sd06 n9 # 15 mu70 sd06 n13
# 16 mu70 sd08 n5 # 17 mu70 sd08 n9 # 18 mu70 sd08 n13
for (i in 1:dim(array.meanscores.ref)[1]) { # 18
  hist(array.meanscores.ref[i,,] ,
       main=paste("Histogram of array.meanscores.ref[",i,"]"),
       xlim=c(-1,0),ylim=c(0,3200000))
}
# ok: mu is most important. for mu=0.7 almost score is gained
## for mu=0.5 little score, and for mu=0.3 the histograms are more level
## besides, in mu=0.3 it is possible to see the effect of n as the second influential factor
## with increasing n, scores decrease
# let's make plots for a 3x3 mosaic
for (i in c(1,2,3,7,8,9,13,14,15)) { # 9 (eliminating sd)
  hist(
    as.vector(array.meanscores.ref[c(i,i+3),,]),
    xlim=c(-1,0),ylim=c(0,6400000),xlab="",ylab="",main=""
  )
}

# verification: array.meanscores.ref matches entries of a file picked from the ref output directory
# > array.meanscores.ref[1,1,1:6]
# [1] -1.0000000 -0.9411765 -0.8823529 -0.8823529 -0.8235294
# [6] -0.8235294

# Best ref results when mu=0.3 , n=5
summary(array.meanscores.ref[c(1,4),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9059 -0.8235 -0.7755 -0.6471 -0.2941

# What is the simulation space like? Are results as good as above?
summary(array.meanscores.sim[params$mu==0.3 & params$n==5,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9294 -0.8824 -0.8369 -0.7882 -0.2941 

# ... so: when the conditions are most favorable for emergence of cooperation, 
#         the decision space summary is better than RL run summary
# Let's see more comparisons...
summary(array.meanscores.ref[c(2,5),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9346 -0.8431 -0.7856 -0.6471 -0.2941
summary(array.meanscores.sim[params$mu==0.3 & params$n==9,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9020 -0.7843 -0.7605 -0.6340 -0.2941
# AHA! Here sim performs better than ref, and ironically, better than the previous sim

summary(array.meanscores.ref[c(3,6),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9457 -0.8235 -0.7884 -0.6471 -0.2941
summary(array.meanscores.sim[params$mu==0.3 & params$n==13,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.8914 -0.7557 -0.7483 -0.6471 -0.3529
# Again, sim is better than ref and also better than previous sim
# I think it is because G has had more states to assess Q with 
#   (personal thought, beyond scope of paper)

summary(array.meanscores.ref[c(7,10),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9597 -0.9412 -0.4706 
summary(array.meanscores.sim[params$mu==0.5 & params$n==5,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9529 -0.9422 -0.9176 -0.4706 

summary(array.meanscores.ref[c(8,11),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9869 -0.9607 -0.9542 -0.5294
summary(array.meanscores.sim[params$mu==0.5 & params$n==9,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9739 -0.9542 -0.9409 -0.9216 -0.5294

summary(array.meanscores.ref[c(9,12),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9910 -0.9640 -0.9638 -0.5294
summary(array.meanscores.sim[params$mu==0.5 & params$n==13,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -0.9729 -0.9457 -0.9242 -0.8869 -0.5882

summary(array.meanscores.ref[c(13,16),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9979 -1.0000 -0.7059 
summary(array.meanscores.sim[params$mu==0.7 & params$n==5,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9963 -1.0000 -0.7059

summary(array.meanscores.ref[c(14,17),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9982 -1.0000 -0.7059
summary(array.meanscores.sim[params$mu==0.7 & params$n==9,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9889 -0.9869 -0.7059

summary(array.meanscores.ref[c(15,18),,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9984 -1.0000 -0.7059
summary(array.meanscores.sim[params$mu==0.7 & params$n==13,,])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.9913 -0.9910 -0.7647

# ok next up: which sim scored better than others?
# first, heatmap of all scores, it confirms that mu has the biggest effect
# new figure 4
## heatmap
### order of rows and columns
chcr <- hclust(dist(t(mat.heatmap.sim.scores)))
rhcr <- hclust(dist(mat.heatmap.sim.scores))

### new heatmaps saved at 1000x600 resolution
heatmap(mat.heatmap.sim.scores , main="Simulation scores heatmap",
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none")
## legend
image(t(as.matrix(1:30)),col=heat.colors(30),
      xaxt="n",yaxt="n")
mtext(c("Min","Max"),
      side=4,las=1,at=c(0,1),line=0.2)
# next, heatmap of ranks, which is better than a column-scaled heat map of scores
# new figure 5
## heatmap
heatmap(mat.heatmap.sim.ranks , main="Simulation ranks heatmap",
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none" , col=cm.colors(90))
## legend
image(t(as.matrix(1:30)),col=cm.colors(30),
      xaxt="n",yaxt="n")
mtext(c("Low","High"),
      side=4,las=1,at=c(0,1),line=0.2)
# alright!
# next question? find the best model!
summary(rowMeans(mat.heatmap.sim.ranks))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.98   29.36   47.63   45.50   59.19   65.78 
which(rowMeans(mat.heatmap.sim.ranks)==max(rowMeans(mat.heatmap.sim.ranks)))
# 6

# The heatmap shows the lower third of the plot has lower ranks
# The dendrogram shows this part corresponds to the lower section of the dendrogram
# Checking the respective rows shows they correspond to TD_ExpectedSARSA and TD_QLearning
# All 15 rows for each algorithm are included in the weak list.
df.perm.indep[rhcr$order[1:30],]
#           algorithm epsilon alpha
# 50 TD_expectedSARSA    0.01   1.0
# 62     TD_QLearning    0.01   0.4
# 72     TD_QLearning    0.20   0.4
# 53 TD_expectedSARSA    0.10   0.6
# 69     TD_QLearning    0.10   0.8
# 70     TD_QLearning    0.10   1.0
# 61     TD_QLearning    0.01   0.2
# 75     TD_QLearning    0.20   1.0
# 51 TD_expectedSARSA    0.10   0.2
# 55 TD_expectedSARSA    0.10   1.0
# 63     TD_QLearning    0.01   0.6
# 68     TD_QLearning    0.10   0.6
# 48 TD_expectedSARSA    0.01   0.6
# 49 TD_expectedSARSA    0.01   0.8
# 66     TD_QLearning    0.10   0.2
# 47 TD_expectedSARSA    0.01   0.4
# 56 TD_expectedSARSA    0.20   0.2
# 57 TD_expectedSARSA    0.20   0.4
# 64     TD_QLearning    0.01   0.8
# 54 TD_expectedSARSA    0.10   0.8
# 52 TD_expectedSARSA    0.10   0.4
# 58 TD_expectedSARSA    0.20   0.6
# 46 TD_expectedSARSA    0.01   0.2
# 73     TD_QLearning    0.20   0.6
# 59 TD_expectedSARSA    0.20   0.8
# 60 TD_expectedSARSA    0.20   1.0
# 65     TD_QLearning    0.01   1.0
# 67     TD_QLearning    0.10   0.4
# 71     TD_QLearning    0.20   0.2
# 74     TD_QLearning    0.20   0.8

# also, it seems that the middle part of the histogram has higher scores in all
df.perm.indep[rhcr$order[31:60],]
vec.highranks = which(sort(rowMeans(mat.heatmap.sim.ranks)) > 60)
df.perm.indep[vec.highranks,]      
# all TD_DoubleSARSA and TD_DoubleExpectedSARSA

# > summary(rowMeans(mat.heatmap.sim.ranks[rhcr$order[1:30],]))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.98   27.04   27.79   28.05   29.33   36.72
# unique(df.perm.indep[rhcr$order[1:30],"algorithm"])
# "TD_expectedSARSA" "TD_QLearning"

# > summary(rowMeans(mat.heatmap.sim.ranks[rhcr$order[31:60],]))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 54.41   59.46   60.63   60.69   62.83   65.78 
# unique(df.perm.indep[rhcr$order[31:60],"algorithm"])
# "TD_DoubleSARSA"         "TD_DoubleExpectedSARSA"

# > summary(rowMeans(mat.heatmap.sim.ranks[rhcr$order[61:90],]))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 42.73   46.46   47.63   47.76   49.38   52.08
# unique(df.perm.indep[rhcr$order[61:90],"algorithm"])
# "TD_SARSA"           "TD_DoubleQLearning"
for (alg in vec.algorithms) {
  print(alg)
  print(summary(rowMeans(mat.heatmap.sim.ranks[df.perm.indep$algorithm==alg,])))
}
# [1] "TD_DoubleExpectedSARSA"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 54.41   57.75   59.72   59.67   60.95   65.78 
# [1] "TD_DoubleQLearning"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 42.73   45.05   47.13   46.62   47.68   49.92 
# [1] "TD_DoubleSARSA"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 58.11   60.49   62.10   61.71   62.91   64.05 
# [1] "TD_expectedSARSA"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.69   26.97   27.55   27.55   28.94   30.70 
# [1] "TD_QLearning"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.98   27.58   28.52   28.55   29.60   36.72 
# [1] "TD_SARSA"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 44.48   47.59   48.69   48.90   50.91   52.08 

# Double learning algorithms performed better than their single-learning counterparts
# Highest ranks: DoubleSARSA, followed by DoubleExpectedSARSA

# What parameter combinations are best?
for (eps in unique.epsilon) {
  print(eps)
  print(summary(rowMeans(mat.heatmap.sim.ranks[df.perm.indep$epsilon==eps,])))
}
# [1] 0.01
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 26.63   30.76   47.22   45.59   58.99   64.05 
# [1] 0.1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.75   29.00   48.35   45.75   57.99   65.78 
# [1] 0.2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.98   28.11   47.31   45.16   59.72   64.19
# Not a substantial difference. Let's focus on the select algs
for (alg in c("TD_DoubleExpectedSARSA","TD_DoubleSARSA")) {
  for (eps in unique.epsilon) {
    print(paste(alg, ", epsilon=", eps))
    print(summary(
      rowMeans(
        mat.heatmap.sim.ranks[df.perm.indep$epsilon==eps &
                              df.perm.indep$algorithm==alg,]
        )
      ))
  }
  
}
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 56.15   57.86   59.77   59.55   60.70   63.28 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 55.51   57.63   58.67   59.60   60.43   65.78 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 54.41   59.72   59.72   59.85   61.19   64.19 
# [1] "TD_DoubleSARSA , epsilon= 0.01"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 59.37   60.41   60.56   61.15   61.37   64.05 
# [1] "TD_DoubleSARSA , epsilon= 0.1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 58.11   61.52   62.85   61.95   63.56   63.72 
# [1] "TD_DoubleSARSA , epsilon= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 59.89   62.10   62.38   62.02   62.78   62.97
# Still not a substantial difference caused by different values of epsilon

for (alp in unique.alpha) {
  print(alp)
  print(summary(rowMeans(mat.heatmap.sim.ranks[df.perm.indep$alpha==alp,])))
}
# [1] 0.2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.68   31.16   45.98   44.96   58.83   65.78 
# [1] 0.4
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 26.91   28.34   47.63   45.34   59.63   62.10 
# [1] 0.6
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.98   30.08   48.07   45.56   56.82   63.56 
# [1] 0.8
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.69   29.10   48.18   45.34   58.79   63.72 
# [1] 1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 26.63   31.24   49.01   46.30   58.05   64.19
# Clear: better results with increasing alpha. Best at alpha=1. Recommended.

for (alg in c("TD_DoubleExpectedSARSA","TD_DoubleSARSA")) {
  for (eps in unique.epsilon) {
    print(paste(alg, ", epsilon=", eps))
    print(summary(
      
        mat.heatmap.sim.ranks[df.perm.indep$epsilon==eps &
                              df.perm.indep$algorithm==alg &
                              df.perm.indep$alpha==1,]
      
    ))
  }
}
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   43.88   62.75   57.86   74.75   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   44.00   63.50   58.67   75.25   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.50   53.38   66.25   64.19   81.25   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.01"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   54.00   67.00   64.05   79.50   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.00   42.00   63.25   58.11   75.38   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.50   51.25   64.75   62.97   79.00   90.00 
# Now we have it clear:
# DoubleSARSA works best with alpha=1 & epsilon=0.01
# DoubleExpectedSARSA works best with alpha=1 & epsilon=0.2

# Getting the same info from the heatmap? NO!
sort(rowMeans(mat.heatmap.sim.ranks))
# [1] 21.98148 22.67593 23.69444 23.75000 26.62963 26.72222
# [7] 26.90741 27.03704 27.03704 27.40741 27.50000 27.54630
# [13] 27.65741 27.68519 27.71296 27.87037 28.01852 28.51852
# [19] 28.80556 28.86111 29.01852 29.29630 29.34259 29.40741
# [25] 29.74074 29.78704 30.70370 30.94444 32.53704 36.72222
# [31] 42.73148 44.14815 44.27778 44.48148 44.86111 45.23148
# [37] 45.38889 46.42593 46.56481 47.05556 47.12963 47.21296
# [43] 47.31481 47.48148 47.56481 47.70370 47.78704 48.03704
# [49] 48.35185 48.65741 48.68519 48.87037 49.54630 49.91667
# [55] 50.12963 50.80556 51.00926 51.58333 51.79630 52.08333
# [61] 54.40741 55.50926 56.14815 57.62963 57.86111 58.11111
# [67] 58.66667 59.37037 59.72222 59.72222 59.76852 59.88889
# [73] 60.40741 60.42593 60.56481 60.70370 61.19444 61.37037
# [79] 61.51852 62.10185 62.37963 62.77778 62.85185 62.97222
# [85] 63.27778 63.55556 63.72222 64.04630 64.19444 65.77778
order(rowMeans(mat.heatmap.sim.ranks))
# [1] 73 71 59 51 65 55 47 54 58 60 62 56 74 50 72 52 67 66 69 53
# [21] 49 57 64 70 48 68 46 63 61 75 16 22 19 76 28 30 86 77 26 29
# [41] 20 25 18 84 27 87 23 81 78 21 88 24 89 17 82 80 90 85 79 83
# [61] 13  9  1  8  5 40 10 32 11 12  2 44 31  7 34  4 14 33 37 42
# [81] 43 41 36 45  3 38 39 35 15  6
df.perm.indep[order(rowMeans(mat.heatmap.sim.ranks)),]
#                 algorithm epsilon alpha
# 73           TD_QLearning    0.20   0.6
# 71           TD_QLearning    0.20   0.2
# 59       TD_expectedSARSA    0.20   0.8
# 51       TD_expectedSARSA    0.10   0.2
# 65           TD_QLearning    0.01   1.0
# 55       TD_expectedSARSA    0.10   1.0
# 47       TD_expectedSARSA    0.01   0.4
# 54       TD_expectedSARSA    0.10   0.8
# 58       TD_expectedSARSA    0.20   0.6
# 60       TD_expectedSARSA    0.20   1.0
# 62           TD_QLearning    0.01   0.4
# 56       TD_expectedSARSA    0.20   0.2
# 74           TD_QLearning    0.20   0.8
# 50       TD_expectedSARSA    0.01   1.0
# 72           TD_QLearning    0.20   0.4
# 52       TD_expectedSARSA    0.10   0.4
# 67           TD_QLearning    0.10   0.4
# 66           TD_QLearning    0.10   0.2
# 69           TD_QLearning    0.10   0.8
# 53       TD_expectedSARSA    0.10   0.6
# 49       TD_expectedSARSA    0.01   0.8
# 57       TD_expectedSARSA    0.20   0.4
# 64           TD_QLearning    0.01   0.8
# 70           TD_QLearning    0.10   1.0
# 48       TD_expectedSARSA    0.01   0.6
# 68           TD_QLearning    0.10   0.6
# 46       TD_expectedSARSA    0.01   0.2
# 63           TD_QLearning    0.01   0.6
# 61           TD_QLearning    0.01   0.2
# 75           TD_QLearning    0.20   1.0
# 16     TD_DoubleQLearning    0.01   0.2
# 22     TD_DoubleQLearning    0.10   0.4
# 19     TD_DoubleQLearning    0.01   0.8
# 76               TD_SARSA    0.01   0.2
# 28     TD_DoubleQLearning    0.20   0.6
# 30     TD_DoubleQLearning    0.20   1.0
# 86               TD_SARSA    0.20   0.2
# 77               TD_SARSA    0.01   0.4
# 26     TD_DoubleQLearning    0.20   0.2
# 29     TD_DoubleQLearning    0.20   0.8
# 20     TD_DoubleQLearning    0.01   1.0
# 25     TD_DoubleQLearning    0.10   1.0
# 18     TD_DoubleQLearning    0.01   0.6
# 84               TD_SARSA    0.10   0.8
# 27     TD_DoubleQLearning    0.20   0.4
# 87               TD_SARSA    0.20   0.4
# 23     TD_DoubleQLearning    0.10   0.6
# 81               TD_SARSA    0.10   0.2
# 78               TD_SARSA    0.01   0.6
# 21     TD_DoubleQLearning    0.10   0.2
# 88               TD_SARSA    0.20   0.6
# 24     TD_DoubleQLearning    0.10   0.8
# 89               TD_SARSA    0.20   0.8
# 17     TD_DoubleQLearning    0.01   0.4
# 82               TD_SARSA    0.10   0.4
# 80               TD_SARSA    0.01   1.0
# 90               TD_SARSA    0.20   1.0
# 85               TD_SARSA    0.10   1.0
# 79               TD_SARSA    0.01   0.8
# 83               TD_SARSA    0.10   0.6
# 13 TD_DoubleExpectedSARSA    0.20   0.6
# 9  TD_DoubleExpectedSARSA    0.10   0.8
# 1  TD_DoubleExpectedSARSA    0.01   0.2
# 8  TD_DoubleExpectedSARSA    0.10   0.6
# 5  TD_DoubleExpectedSARSA    0.01   1.0
# 40         TD_DoubleSARSA    0.10   1.0
# 10 TD_DoubleExpectedSARSA    0.10   1.0
# 32         TD_DoubleSARSA    0.01   0.4
# 11 TD_DoubleExpectedSARSA    0.20   0.2
# 12 TD_DoubleExpectedSARSA    0.20   0.4
# 2  TD_DoubleExpectedSARSA    0.01   0.4
# 44         TD_DoubleSARSA    0.20   0.8
# 31         TD_DoubleSARSA    0.01   0.2
# 7  TD_DoubleExpectedSARSA    0.10   0.4
# 34         TD_DoubleSARSA    0.01   0.8
# 4  TD_DoubleExpectedSARSA    0.01   0.8
# 14 TD_DoubleExpectedSARSA    0.20   0.8
# 33         TD_DoubleSARSA    0.01   0.6
# 37         TD_DoubleSARSA    0.10   0.4
# 42         TD_DoubleSARSA    0.20   0.4
# 43         TD_DoubleSARSA    0.20   0.6
# 41         TD_DoubleSARSA    0.20   0.2
# 36         TD_DoubleSARSA    0.10   0.2
# 45         TD_DoubleSARSA    0.20   1.0
# 3  TD_DoubleExpectedSARSA    0.01   0.6
# 38         TD_DoubleSARSA    0.10   0.6
# 39         TD_DoubleSARSA    0.10   0.8
# 35         TD_DoubleSARSA    0.01   1.0
# 15 TD_DoubleExpectedSARSA    0.20   1.0
# 6  TD_DoubleExpectedSARSA    0.10   0.2
df.perm.indep.results = df.perm.indep[order(rowMeans(mat.heatmap.sim.ranks)),]
df.perm.indep.results[,"meanRank"] = sort(rowMeans(mat.heatmap.sim.ranks))


for (alg in c("TD_DoubleExpectedSARSA","TD_DoubleSARSA")) {
  for (eps in unique.epsilon) {
    for (alp in unique.alpha) {
    print(paste(alg, ", epsilon=", eps, ", alpha=", alp))
    print(summary(
      mat.heatmap.sim.ranks[df.perm.indep$epsilon==eps &
                              df.perm.indep$algorithm==alg &
                              df.perm.indep$alpha==alp,]
    ))
    }
  }
}
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01 , alpha= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00   42.25   61.25   56.15   73.00   87.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01 , alpha= 0.4"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   40.50   67.50   59.77   80.00   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01 , alpha= 0.6"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00   56.12   69.00   63.28   78.00   89.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01 , alpha= 0.8"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.50   44.00   64.50   60.70   76.88   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.01 , alpha= 1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   43.88   62.75   57.86   74.75   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1 , alpha= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   59.62   70.50   65.78   80.62   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1 , alpha= 0.4"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   48.50   66.25   60.43   75.00   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1 , alpha= 0.6"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   43.88   63.50   57.63   71.00   89.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1 , alpha= 0.8"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.50   39.62   60.00   55.51   77.75   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.1 , alpha= 1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   44.00   63.50   58.67   75.25   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2 , alpha= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   42.38   60.50   59.72   76.88   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2 , alpha= 0.4"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   47.62   67.00   59.72   73.88   87.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2 , alpha= 0.6"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.00   40.50   57.25   54.41   73.50   89.50 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2 , alpha= 0.8"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   46.25   64.25   61.19   81.00   90.00 
# [1] "TD_DoubleExpectedSARSA , epsilon= 0.2 , alpha= 1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.50   53.38   66.25   64.19   81.25   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.01 , alpha= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   45.25   65.00   60.41   80.12   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.01 , alpha= 0.4"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   50.00   62.50   59.37   77.75   89.00 
# [1] "TD_DoubleSARSA , epsilon= 0.01 , alpha= 0.6"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.50   46.75   65.50   61.37   80.75   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.01 , alpha= 0.8"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00   48.62   67.50   60.56   79.75   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.01 , alpha= 1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   54.00   67.00   64.05   79.50   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.1 , alpha= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   47.12   67.00   62.85   82.50   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.1 , alpha= 0.4"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   43.00   75.00   61.52   83.00   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.1 , alpha= 0.6"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.50   55.12   67.00   63.56   78.75   88.00 
# [1] "TD_DoubleSARSA , epsilon= 0.1 , alpha= 0.8"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   50.62   63.25   63.72   80.00   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.1 , alpha= 1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.00   42.00   63.25   58.11   75.38   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.2 , alpha= 0.2"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.00   50.50   70.75   62.78   76.00   89.00 
# [1] "TD_DoubleSARSA , epsilon= 0.2 , alpha= 0.4"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.0    46.0    66.5    62.1    83.0    90.0 
# [1] "TD_DoubleSARSA , epsilon= 0.2 , alpha= 0.6"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   48.00   66.50   62.38   78.75   90.00 
# [1] "TD_DoubleSARSA , epsilon= 0.2 , alpha= 0.8"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   42.00   62.50   59.89   78.50   89.00 
# [1] "TD_DoubleSARSA , epsilon= 0.2 , alpha= 1"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.50   51.25   64.75   62.97   79.00   90.00 

# lastly: score-time graph for 2 algorithms and a random baseline
# series of meanscores for 17 time steps
# + for algorithms: added series for alpha
mat.timescores.DS = array.timeSteps[,,"TD_DoubleSARSA"]
mat.timescores.DXS = array.timeSteps[,,"TD_DoubleExpectedSARSA"]
plot(1:17,type="n",xlim=c(1,17),ylim=c(-1,0),
     xlab="Time steps",ylab="Rewards")

vec.cols = c("red","blue")
#mat.rgb = rbind(c(0.5,0,0),c(0,0,0.5))
mat.rgb = rbind(c(1,0,0),c(0,0,1))
colnames(mat.rgb)=c("r","g","b")
names(vec.cols) = c("TD_DoubleSARSA","TD_DoubleExpectedSARSA")
for (i in 1:length(unique.alpha)) { # 5
  for (j in 1:length(vec.cols)) { # 2
    alp = unique.alpha[i]
    alg=names(vec.cols)[j]
    temp.data=array.timeSteps[which(params$alpha==alp),,alg]
    for (h in 1:dim(temp.data)[1]) { # 162
      temp.line=temp.data[h,]
      lines(temp.line,col=rgb(r=mat.rgb[j,"r"],
                              g=mat.rgb[j,"g"],
                              b=mat.rgb[j,"b"],
                              alpha = 0.1))
    }
    #points(colMeans(temp.data),pch=i,col=vec.cols[j])
    #lines(colMeans(temp.data),col=vec.cols[j])
  }
}
legend("topleft",
       legend=c("Double SARSA","Double Expected SARSA"),
       #col=c(rgb(.5,0,0),rgb(0,0,.5)),
       col=c(rgb(1,0,0),rgb(0,0,1)),
       cex=0.8,
       lty=1,
       bty="n",
       y.intersp=0.3 , x.intersp= 0.1,
       seg.len=0.5)


# Ok
# Let's add the baseline (random)
folder.runs.ref.random = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest15random/output/"
list.runs.ref.random = list.files(folder.runs.ref.random)
file.template.ref.random = paste(folder.runs.ref.random,list.runs.ref.random[1],"/ref1random.txt",sep="")
template.ref.random = read.csv(file.template.ref.random, sep=" ", header=FALSE) # 400x17
template.ref.random = as.matrix(template.ref.random)
array.timeSteps.ref.random = array(data=NA,
                            dim=c(
                              nrow(params), # 810
                              ncol(template.ref.random), #17
                              nrow(template.thresholds)), # 50
                            dimnames=list(
                              1:nrow(params),
                              1:ncol(template.ref.random),
                              1:nrow(template.thresholds))
)

# loop
for (mu in unique.mu) {
  str.mu = format(mu,nsmall = 2)
  str.mu = substr(str.mu,3,nchar(str.mu))
  str.mu = paste("mu",str.mu,sep="")
  for (sigma in unique.sigma) {
    str.sigma = format(sigma,nsmall = 2)
    str.sigma = substr(str.sigma,3,nchar(str.sigma))
    str.sigma = paste("sd",str.sigma,sep="")
    for (n in unique.n) {
      str.n = paste("n",n,sep="")
      index.temp = numeric()
      # find respective subfolder in output
      for (j in 1:length(list.runs.ref.random)) {
        run = list.runs.ref.random[j]
        if (grepl(str.mu,run) & grepl(str.sigma,run) & grepl(str.n,run))
        {
          index.temp = j 
          break
        }
      }
      # list of files in respective output folder
      list.files.temp.ref = list.files(paste(folder.runs.ref.random,list.runs.ref.random[index.temp],sep=""),
                                       full.names = TRUE)
      
      # all params rows corresponding to this ref run
      index.params = which(params$mu==mu & params$sigma==sigma & params$n==n) 
      
      for (i in 1:dim(array.timeSteps.ref.random)[3]) # each 'page' in the 3d array is associated with a run
      {
        index.rewards.ref = which(grepl(paste("ref",i,"random",sep=""),list.files.temp.ref))
        file.rewards.ref = list.files.temp.ref[index.rewards.ref]
        rewards.ref = read.csv(file.rewards.ref,sep=" ",header=FALSE)
        rewards.ref = as.matrix(rewards.ref) # now it is converted from char to double
        # writing in array.timeSteps.ref
        for (k in index.params) {
          array.timeSteps.ref.random[k,,i] = colMeans(rewards.ref)
        }
      }
    }
  }
}
plot(apply(array.timeSteps.ref.random,2,mean),ylim=c(-1,0))
points(apply(array.timeSteps.sim[,,,"TD_DoubleSARSA"],2,mean),ylim=c(-1,0),col="blue")
points(apply(array.timeSteps.sim[,,,"TD_DoubleExpectedSARSA"],2,mean),ylim=c(-1,0),col="green")
save(array.timeSteps.ref.random,file = paste(folder.social,"arrayTimeStepsRefRandom",sep=""))

# read all ref.random files 18x50x400 =360k episodes
# (save them as 18x20k , just in case)
mat.17.ref = matrix(NA,nrow=18,ncol=20000)
# take note of 17'th time step 360k points
for (i in 1:18) {
  vec.temp = numeric()
  folder.temp = paste(folder.runs.ref.random,
                      list.runs.ref.random[i],"/",sep="")
  list.temp = list.files(folder.temp , full.names = TRUE)
  list.temp = list.temp[which(grepl("random.txt",list.temp))]
  for (file.temp in list.temp) {
    rewards.temp = read.csv(file.temp, sep=" ", header=FALSE)
    rewards.temp = as.matrix(rewards.temp)
    vec.temp = c(vec.temp , rewards.temp[,17])
  }
  mat.17.ref[i,] = vec.temp
}
# > summary(as.vector(mat.17.ref))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -1.0000 -0.7081 -0.5385  0.0000
# boxplot
boxplot(as.vector(mat.17.ref),ylim=c(-1,0))

vec.17.ref.40500 = sample(as.vector(mat.17.ref),40500)
boxplot(vec.17.ref.40500,ylim=c(-1,0),col="brown")

# read all sim files of selected algorithms 810x50=4.05k each
mat.17.sim.DS = matrix(NA,nrow=810,ncol=50)
mat.17.sim.DXS= matrix(NA,nrow=810,ncol=50)

array.17.sim = array(data=NA,
                             dim=c(nrow(params),  # 810
                                   num.runs.per.folder, # 50
                                   length(vec.algorithms)), # 6
                             dimnames=list(1:nrow(params),
                                           1:num.runs.per.folder,
                                           vec.algorithms)
)
folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/outputN2vomxN2vom_4k/"
list.runs.sim = list.files(folder.runs.sim,full.names = TRUE) # character array

for (i in 1:nrow(params)) { # 810
  num.mu = params[i, "mu"]
  str.mu = format(num.mu, nsmall = 2)
  str.mu = substr(str.mu, 3, nchar(str.mu))
  str.mu = paste("mu", str.mu, sep = "")
  num.sigma = params[i, "sigma"]
  str.sigma = format(num.sigma, nsmall = 2)
  str.sigma = substr(str.sigma, 3, nchar(str.sigma))
  str.sigma = paste("sd", str.sigma, sep = "")
  num.gamma = params[i, "gamma"]
  str.gamma = format(num.gamma / 10, nsmall = 3)
  str.gamma = substr(str.gamma, 3, nchar(str.gamma))
  str.gamma = paste("g", str.gamma, sep = "")
  num.n = params[i, "n"]
  str.n = paste("n", num.n, sep = "")
  num.epsilon = params[i, "epsilon"]
  str.epsilon = format(num.epsilon / 10, nsmall = 3)
  str.epsilon = substr(str.epsilon, 3, nchar(str.epsilon))
  str.epsilon = paste("e", str.epsilon, sep = "")
  num.alpha = params[i, "alpha"]
  str.alpha = format(num.alpha / 10, nsmall = 3)
  str.alpha = substr(str.alpha, 3, nchar(str.alpha))
  str.alpha = paste("a", str.alpha, sep = "")
  index.sim.folder = which(
    grepl(str.mu, list.runs.sim) &
      grepl(str.sigma, list.runs.sim) &
      grepl(str.gamma, list.runs.sim) &
      grepl(str.n, list.runs.sim) &
      grepl(str.epsilon, list.runs.sim) &
      grepl(str.alpha, list.runs.sim)
  )
  folder.temp = list.runs.sim[index.sim.folder]
  list.temp = list.files(folder.temp, full.names = TRUE)
  
  for (algorithm in vec.algorithms) { # 6
    index.temp = which(grepl("rewards",list.temp) & 
                         grepl(algorithm,list.temp))
    file.rewards = list.temp[index.temp]
    rewards = read.csv(file.rewards,sep=" ",header=FALSE)
    rewards = as.matrix(rewards)
    array.17.sim[i,,algorithm] = rewards[,17]
  }
}
mat.17.sim.DS = array.17.sim[,,"TD_DoubleSARSA"]
mat.17.sim.DXS= array.17.sim[,,"TD_DoubleExpectedSARSA"]
# boxplot
boxplot(as.vector(mat.17.sim.DS),ylim=c(-1,0),col="red")
boxplot(as.vector(mat.17.sim.DXS),ylim=c(-1,0),col="blue")

#
df.fig7.40500 = data.frame("Double SARSA"=as.vector(mat.17.sim.DS),
                          "Double Expected SARSA"=as.vector(mat.17.sim.DXS),
                          "Baseline"=vec.17.ref.40500)
boxplot(df.fig7.40500,ylim=c(-1,0),col="white")
for (i in 1:3) {
  testjitter = jitter(rep(i,40500),amount=0.25)
  points(testjitter , df.fig7.40500[,i] , pch=20 , col=rgb(r=0.5,g=0,b=0,alpha=0.1))
}

for (i in 1:3) {
  hist(df.fig7.40500[,i],main=colnames(df.fig7.40500)[i],
       xlim=c(-1,0),ylim=c(0,25000),xlab="")
}
# chose histograms for fig7 
# save 600x600

# figure 8:
# box plots of 2 algorithms and baseline for 3 levels of mu
# so in all 9 box plots
df.fig8 = data.frame("DXSm3"=as.vector(mat.17.sim.DXS[which(params$mu==0.3),]),
                     "DSm3" =as.vector(mat.17.sim.DS[which(params$mu==0.3),]),
                     "basm3"=sample(as.vector(mat.17.ref[1:6,]),13500),
                     "break1"=rep(NA,13500),
                     "break11"=rep(NA,13500),
                     "DXSm5"=as.vector(mat.17.sim.DXS[which(params$mu==0.5),]),
                     "DSm5" =as.vector(mat.17.sim.DS[which(params$mu==0.5),]),
                     "basm5"=sample(as.vector(mat.17.ref[7:12,]),13500),
                     "break2"=rep(NA,13500),
                     "break22"=rep(NA,13500),
                     "DXSm7"=as.vector(mat.17.sim.DXS[which(params$mu==0.7),]),
                     "DSm7" =as.vector(mat.17.sim.DS[which(params$mu==0.7),]),
                     "basm7"=sample(as.vector(mat.17.ref[13:18,]),13500) )

oldpar=par()
par(mar=c(8,4,2,1))
boxplot(df.fig8,xaxt="n",boxwex=0.5,ylab="Rewards",cex.axis=0.8)
axis(side=1,line=0,las=2,
     c("Doub.SARSA","Doub.Exp.SARSA","Baseline","","",
       "Doub.SARSA","Doub.Exp.SARSA","Baseline","","",
       "Doub.SARSA","Doub.Exp.SARSA","Baseline"),
     at=1:13 , tick=FALSE)
axis(side=3,line=0,tick=FALSE,
     c(expression(mu==0.3),
       expression(mu==0.5),
       expression(mu==0.7)),
     at=c(2,7,12))
par(oldpar)
