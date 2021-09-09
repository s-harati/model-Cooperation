# In this code I compare several batches of runs with
# different settings: numLevels, and episodes per run
# For each batch, I want summary of:
## 1) Mean episode score
## 2) Step 17 score

# Preparations
folder.social = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/analysis/"

file.array.sim = paste(folder.social,"arrayTimeStepsSim",sep="")
load(file.array.sim) # array 810 x 17 x 50 x 6
file.array.ref = paste(folder.social,"arrayTimeStepsRef",sep="")
load(file.array.ref) # array 810 x 17 x 50
file.params = paste(folder.social,"paramsPermutations.csv",sep="")
params = read.csv(file.params) # data.frame

vec.algorithms = dimnames(array.timeSteps.sim)[[4]]
steps.per.episode = dim(array.timeSteps.sim)[2]

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

# Reading run outputs
#folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/output3x3_400/"
#folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/output3x3_4k/"
#folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/output7x7_40k/"
#folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/output3xN2vom_400/"
#folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/output3xN2vom_4k/"
#folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/outputN2vomxN2vom_400/"
folder.runs.sim = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/outputN2vomxN2vom_4k/"

# Analyzing outputs
array.meanscores.sim = array(data=NA,
                             dim=c(nrow(params),  # 810
                                   num.runs.per.folder, # 50
                                   length(vec.algorithms)), # 6
                             dimnames=list(1:nrow(params),
                                           1:num.runs.per.folder,
                                           vec.algorithms)
)

array.lastscores.sim = array(data=NA,
                             dim=c(nrow(params),  # 810
                                   num.runs.per.folder, # 50
                                   length(vec.algorithms)), # 6
                             dimnames=list(1:nrow(params),
                                           1:num.runs.per.folder,
                                           vec.algorithms)
)

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
    
    array.lastscores.sim[i,,algorithm] = rewards[,steps.per.episode]
  }
}

# Summarizing results
summary(array.meanscores.sim)
#######    3x3_400    #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9739 -0.9323 -0.9059 -0.2941 

#######    3x3_4k     #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9739 -0.9317 -0.9059 -0.2941 

#######    7x7_40k    #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9729 -0.9019 -0.8733 -0.2941 

#######  3xN2vom_400  #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9739 -0.9335 -0.9085 -0.2941 

#######  3xN2vom_4k   #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9729 -0.9332 -0.9085 -0.2941 

###### N2vomxN2vom_400 ######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9529 -0.9052 -0.8824 -0.2941

###### N2vomxN2vom_4k  ######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9529 -0.9033 -0.8824 -0.2941

# FOR COMPARISON: PREVIOUSLY CALCULATED REF FILE:
# > summary(array.meanscores.ref)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.9910 -0.9143 -0.8941 -0.2941

summary(array.lastscores.sim)
#######    3x3_400    #######
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8889 -0.7217 -0.6000  0.0000

#######    3x3_4k     #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8889 -0.7200 -0.6000  0.0000

#######    7x7_40k    #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8889 -0.6708  0.0000  0.0000

#######  3xN2vom_400  #######
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8889 -0.7272 -0.6000  0.0000

#######  3xN2vom_4k   #######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8889 -0.7283 -0.6154  0.0000 

###### N2vomxN2vom_400 ######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8000 -0.6442  0.0000  0.0000

###### N2vomxN2vom_4k  ######
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0000 -1.0000 -0.8000 -0.6363  0.0000  0.0000 