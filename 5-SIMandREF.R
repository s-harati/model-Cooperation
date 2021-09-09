# program to read simulation and reference data arrays and
# 1a-produce heatmap for simulation response variables vs. independent variables (scores)
# 1b-  "         "     "       "       "         "       "       "         "     (ranks)
# 2a-  "         "     "  sim - ref    "         "       "       "         "     (scores)
# 2b-  "         "     "       "       "         "       "       "         "     (ranks)

folder.social = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/analysis/"

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

df.perm.indep = data.frame(algorithm=character(),epsilon=numeric(),alpha=numeric(),
                           stringsAsFactors = FALSE)
index.temp = 0
for (algorithm in vec.algorithms) {
  for (epsilon in unique.epsilon) {
    for (alpha in unique.alpha) {
      index.temp = index.temp + 1
      df.perm.indep[index.temp,"algorithm"] = algorithm
      df.perm.indep[index.temp,"epsilon"] = epsilon
      df.perm.indep[index.temp,"alpha"] = alpha
    }
  }
}
rm(index.temp)
  
df.perm.resp = data.frame(mu=numeric(),sigma=numeric(),n=integer(),gamma=numeric(),
                           stringsAsFactors = FALSE)
index.temp = 0
for (mu in unique.mu) {
  for (sigma in unique.sigma) {
    for (n in unique.n) {
      for (gamma in unique.gamma) {
        index.temp = index.temp + 1
        df.perm.resp[index.temp,"mu"] = mu
        df.perm.resp[index.temp,"sigma"] = sigma
        df.perm.resp[index.temp,"n"] = n
        df.perm.resp[index.temp,"gamma"] = gamma
      }
    }
  }
}
rm(index.temp)

# create a table with rows for independent combinations and columns for response combinations
# Total combinations are nrow(params) x nAlgorithms=810x6=4860 = 90x54
mat.heatmap.sim.scores =      matrix(NA,nrow=nrow(df.perm.indep),ncol=nrow(df.perm.resp))
mat.heatmap.sim.ranks =       matrix(NA,nrow=nrow(df.perm.indep),ncol=nrow(df.perm.resp))
mat.heatmap.simNref.scores =  matrix(NA,nrow=nrow(df.perm.indep),ncol=nrow(df.perm.resp))
mat.heatmap.simNref.ranks =   matrix(NA,nrow=nrow(df.perm.indep),ncol=nrow(df.perm.resp))

for (i in 1:nrow(df.perm.indep)) {
  algorithm = df.perm.indep$algorithm[i]
  for (j in 1:nrow(df.perm.resp)) {
    index.params = which((params$mu==df.perm.resp$mu[j]) &
                         (params$sigma==df.perm.resp$sigma[j]) &
                         (params$n==df.perm.resp$n[j]) &
                         (params$gamma==df.perm.resp$gamma[j]) &
                         (params$epsilon==df.perm.indep$epsilon[i]) &
                         (params$alpha==df.perm.indep$alpha[i]) )
    # cell (i,j) of the heatmap table corresponds to index.params and algorithm in array.timeSteps
    # it is the average of scores of all timeSteps of all runs with those parameters.
    mat.heatmap.sim.scores[i,j] = mean(array.timeSteps.sim[index.params,,,algorithm])
    mat.heatmap.simNref.scores[i,j] = mat.heatmap.sim.scores[i,j] - mean(array.timeSteps.ref[index.params,,])
  }
}

mat.heatmap.sim.ranks     = apply(mat.heatmap.sim.scores,2,rank)
mat.heatmap.simNref.ranks = apply(mat.heatmap.simNref.scores,2,rank)

chcr <- hclust(dist(t(mat.heatmap.sim.scores)))
rhcr <- hclust(dist(mat.heatmap.sim.scores))
# new heatmaps saved at 1000x600 resolution
heatmap(mat.heatmap.sim.scores , main="Simulation scores heatmap",
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        zlim=c(-1,0) , scale="none")
lgd_ = rep(NA, 11)
lgd_[c(1,11)] = c("low","high")
legend("bottomright",
       legend = rev(lgd_),
       fill = rev(colorRampPalette(colors = heat.colors(90))(11)),
       border = NA, bty="n" , #horiz=TRUE ,
       y.intersp = 0.4, #yjust = 0.5 , xjust = 0.5 ,
       cex = 0.5, text.font = 0.5)
heatmap(mat.heatmap.sim.ranks , main="Simulation ranks heatmap" ,
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none")
lgd_ = rep(NA, 11)
lgd_[c(1,11)] = c("low","high")
legend("bottomright",
       legend = rev(lgd_),
       fill = rev(colorRampPalette(colors = heat.colors(90))(11)),
       border = NA, bty="n" , #horiz=TRUE ,
       y.intersp = 0.4, #yjust = 0.5 , xjust = 0.5 ,
       cex = 0.5, text.font = 0.5)
heatmap(mat.heatmap.simNref.scores , main="Simulation-reference scores heatmap",
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none") # removed , zlim=c(-1,0)
lgd_ = rep(NA, 11)
lgd_[c(1,11)] = c("low","high")
legend("bottomright",
       legend = rev(lgd_),
       fill = rev(colorRampPalette(colors = heat.colors(90))(11)),
       border = NA, bty="n" , #horiz=TRUE ,
       y.intersp = 0.4, #yjust = 0.5 , xjust = 0.5 ,
       cex = 0.5, text.font = 0.5)
heatmap(mat.heatmap.simNref.ranks , main="Simulation-reference ranks heatmap" , 
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none")
lgd_ = rep(NA, 11)
lgd_[c(1,11)] = c("low","high")
legend("bottomright",
       legend = rev(lgd_),
       fill = rev(colorRampPalette(colors = heat.colors(90))(11)),
       border = NA, bty="n" , #horiz=TRUE ,
       y.intersp = 0.4, #yjust = 0.5 , xjust = 0.5 ,
       cex = 0.5, text.font = 0.5)


image(mat.heatmap.sim.scores , main="Simulation scores heatmap",
      zlim=c(-1,0))
image(mat.heatmap.sim.ranks , main="Simulation ranks heatmap")
image(mat.heatmap.simNref.scores , main="Simulation-reference scores heatmap",
      zlim=c(-1,0))
image(mat.heatmap.simNref.ranks , main="Simulation-reference ranks heatmap")


mat.heatmap.simNref.ranks.rowSums = cbind(mat.heatmap.simNref.ranks ,
                                          rowSums(mat.heatmap.simNref.ranks))
rownames(mat.heatmap.simNref.ranks.rowSums) = 1:nrow(mat.heatmap.simNref.ranks.rowSums)

index.order = order(mat.heatmap.simNref.ranks.rowSums[,ncol(mat.heatmap.simNref.ranks.rowSums)],decreasing = TRUE)
mat.heatmap.simNref.ranks.rowSums = mat.heatmap.simNref.ranks.rowSums[index.order,]

# so far we verified average model performance vs average reference results
# and selected (35) DoubleSARSA e.01 a1 and (5) DoubleExpectedSARSA e.01 a1
# now we want to know about the distribution
# it will be good to know in what percentile of the reference data
# our simulation runs appear.
# for each individual run, there are 2^17 ref points and 1 sim point
# 50 such individual runs per setting
# for each of them we note a percentile number
# makes sense.
# then we show variability of these percentiles for the entire dataset
# ok we have 18x50=900 ref runs, and for each we have 270 sim runs
# 3(gamma) x 3(epsilon) x 5(alpha) x 6(algorithms) = 270
# 900x270=1000x3^5=243000 sim runs

folder.runs.ref = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest15/output/"
list.runs.ref = list.files(folder.runs.ref)
file.template.ref = paste(folder.runs.ref,list.runs.ref[1],"/ref1.txt",sep="")
template.ref = scan(file.template.ref, what=double(), sep=" ")
# reminder: file ref1.txt includes average per timestep scores for 2^17 runs
# reminder: the last entry is NA and must be removed
template.ref = template.ref[-length(template.ref)] # last entry removed, now it is a vector and no longer a matrix

file.template.thresholds = paste(folder.runs.ref,list.runs.ref[1],"/thresholds.txt",sep="")
template.thresholds = read.csv(file.template.thresholds, sep=" ",header=FALSE)
template.thresholds = as.matrix(template.thresholds)

num.ref.episodes.per.run = length(template.ref) # 2^16 in new runs
num.runs.per.folder = nrow(template.thresholds) # 50 files
num.ref.folders = length(list.runs.ref) # 18 folders
 
array.meanscores.ref = array(data=NA,
                             dim=c(num.ref.folders, # 18
                                   num.runs.per.folder, # 50
                                   num.ref.episodes.per.run) # 2^17
                             )  

params.with.ref = params
params.with.ref[,"refFolderCode"] = integer()
params.with.ref[,"refFolderName"] = character()
index.temp = 0
list.runs.ref.ordered = character()
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
      index.ref.folder = which(grepl(str.mu,list.runs.ref) & 
                                 grepl(str.sigma,list.runs.ref) & 
                                 grepl(str.n,list.runs.ref))
      index.params = which((params$mu == mu) & 
                             (params$sigma == sigma) & 
                             (params$n == n))
      index.temp = index.temp + 1
      params.with.ref[index.params,"refFolderCode"] = index.temp
      params.with.ref[index.params,"refFolderName"] = list.runs.ref[index.ref.folder]
      list.runs.ref.ordered[index.temp] = paste(folder.runs.ref,list.runs.ref[index.ref.folder],sep="")
    }
  }
}
rm(index.temp)

for (i in 1:num.ref.folders) {
  folder.temp = list.runs.ref.ordered[i]
  list.temp = list.files(folder.temp,full.names = TRUE)
  for (j in 1: num.runs.per.folder) {
    index.temp = which(grepl(paste("ref",j,".txt",sep=""),list.temp))
    file.ref.temp = list.temp[index.temp]
    ref.temp = scan(file.ref.temp,what=double(),sep=" ")
    ref.temp = ref.temp[-length(ref.temp)]
    array.meanscores.ref[i,j,] = ref.temp
  }
}
rm(index.temp)
# > summary(array.meanscores.ref) # OLD, NO LONGER VALID!
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.99100 -0.41180 -0.51600 -0.10290 -0.05882 
# New runs with correction in Registrar.resetStats() , NEW, VALID:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -1.0000 -1.0000 -0.9910 -0.9143 -0.8941 -0.2941 
save(array.meanscores.ref,file=paste(folder.social,"arrayMeanScoresRef",sep=""))

array.meanscores.sim = array(data=NA,
                             dim=c(nrow(params),  # 810
                                   num.runs.per.folder, # 50
                                   length(vec.algorithms)), # 6
                             dimnames=list(1:nrow(params),
                                           1:num.runs.per.folder,
                                           vec.algorithms)
)
array.percentiles.sim = array(data=NA,
                              dim=c(nrow(params),  # 810
                                    num.runs.per.folder, # 50
                                    length(vec.algorithms)), # 6
                              dimnames=list(1:nrow(params),
                                            1:num.runs.per.folder,
                                            vec.algorithms)
)
# folder.runs = "/Users/Saeed/eclipse-workspace/cooptest13/output/"
# folder.runs = "/Users/Saeed/eclipse-workspace/cooptest14/output/"
# folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/frMac/output/7x7_40k/"
folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/outputN2vomxN2vom_4k/"

list.runs.sim = list.files(folder.runs.sim,full.names = TRUE) # character array

for (i in 1:nrow(params)) { # 810
  # identify folder name from params
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
    array.meanscores.sim[i,,algorithm] = rowMeans(rewards)
  }
}

for (i in 1:num.ref.folders) { # 18
  index.temp = which(params.with.ref$refFolderCode == i)
  for (j in 1:num.runs.per.folder) { # 50
    ref.percentiles = ecdf(array.meanscores.ref[i,j,])
    for (algorithm in vec.algorithms) {
      array.percentiles.sim[index.temp,j,algorithm] = 
        ref.percentiles(array.meanscores.sim[index.temp,j,algorithm])
    }
  }
}
save(array.meanscores.sim,file=paste(folder.social,"arrayMeanScoresSim",sep=""))
save(array.percentiles.sim,file=paste(folder.social,"arrayPercentilesSim",sep=""))

#################
#### Figures ####
#################
# Figure 3 – histogram of overall scores of episodes in reference and simulation datasets (15)
# 3a
hist(array.meanscores.ref, main="Histogram of reference scores",xlab="",xlim=c(-1,0))
# saved 400x400
# 3b
hist(array.meanscores.sim, main="Histogram of simulation scores",xlab="",xlim=c(-1,0))
# saved 400x400
# Figure 4 – histograms of scores of simulations (1 plot for each simulation) (16)
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
# Figure 5 – heatmaps of overall scores of simulations with and without respect to reference (17)
# 6a: lowest scores (red column) for mu=0.7 . highest scores (top one-third zone) for epsilon=0.01 
heatmap(mat.heatmap.sim.scores , main="Simulation scores heatmap",
        zlim=c(-1,0) , scale="none")
# 6b: basically same order as 6a, but with respect to reference we see that the difference between mu=0.7 and other mu's is not as substantial as suggested in 6a
heatmap(mat.heatmap.simNref.scores , main="Simulation vs. reference scores heatmap",
        zlim=c(-1,0) , scale="none")
# both saved 777x800
#
# Figure 6 – heatmap of ranks of simulations (18)
heatmap(mat.heatmap.sim.ranks , main="Simulation ranks heatmap" , 
        scale="none")
# saved 777x800
#
# Figure 7 – boxplots of time step scores in selected algorithms [REMOVED: and reference dataset] (19)
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

# Figure 8 boxplots of overall scores in selected algorithms and reference dataset (20)
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