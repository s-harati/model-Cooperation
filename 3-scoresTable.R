# code to read scores from run outputs and add them to the paramsPermutations table
# alright let's do this
# first the code reads and loads the data frame of paramsPermutations
# then it reads the output folder.
# then it creates 6 vectors for the 6 dependent variables, scores of the 6 algorithms
#  the index of these vectors is the same as the rows of params data frame
# for each subfolder in output:
## for each of the 6 algorithms:
### find the output file

### for each timestep 
#### calculate the average score of all runs (sum/numruns)
### calculate the average of the above-calculated averages (sum/numsteps)

### (the above block is equivalent to calculating the overal average of the file BUT
###  let's keep it as above because we can later compare each row (run) with reference)

### write the result in the respective dray (params rownum) of the respective array (algorithm scores)
## end for algorithm
# end for subfolder


# first the code reads and loads the data frame of paramsPermutations
# load file of all codes
folder.social = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/analysis/"
file.params = paste(folder.social,"paramsPermutations.csv",sep="")
params = read.csv(file.params) # data.frame

# then it reads the output folder.
# locate directory of all runs
# folder.runs = "/Users/Saeed/eclipse-workspace/cooptest13/output/"
# folder.runs = "/Users/Saeed/eclipse-workspace/cooptest14/output/"
# folder.runs = "/Users/Saeed/eclipse-workspace/cooptest15/output/"
#folder.runs = "/Users/Saeed/Desktop/output/" #tested ok
#folder.runs = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/frMac/output/7x7_40k/"
folder.runs = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/outputN2vomxN2vom_4k/"

#locate list of all runs
list.runs = list.files(folder.runs) # character array
# identify number of rows in run files
file.template = paste(folder.runs,list.runs[1],"/TD_SARSA_rewards.txt",sep="")
template = read.csv(file.template, sep=" ",header=FALSE)
template = as.matrix(template)
# template.nrow = nrow(template) # 49 error! fix: header=FALSE. done (see above)
# then it creates a matrix with 6 columns for the 6 dependent variables, scores of the 6 algorithms
#  the row index of this matrix is the same as the rows of params data frame
vec.algorithms = c("TD_DoubleExpectedSARSA","TD_DoubleQLearning","TD_DoubleSARSA",
                   "TD_expectedSARSA","TD_QLearning","TD_SARSA")
# matrix of overall mean scores per algorithm and parameter setting
mat.scores = matrix(data=NA,ncol=length(vec.algorithms),nrow=nrow(params),
                    dimnames=list(1:nrow(params),vec.algorithms))
# added next:
# array of mean score of each time step per algorithm and parameter setting
array.timeSteps = array(data=NA,
                        dim=c(nrow(params),
                              ncol(template),
                              length(vec.algorithms)),
                        dimnames=list(1:nrow(params),
                                      1:ncol(template),
                                      vec.algorithms)
                        )
# new addition: array of scores of each run, similar to that defined for ref
array.timeSteps.sim = array(data=NA,
                            dim=c(nrow(params),  # 810
                                  ncol(template), # 17
                                  nrow(template), # 50
                                  length(vec.algorithms)), # 6
                            dimnames=list(1:nrow(params),
                                          1:ncol(template),
                                          1:nrow(template),
                                          vec.algorithms)
                            )

# for each row in the params table
for (i in 1:nrow(params)) {
  # identify folder name from params
  num.mu = params[i,"mu"]
  str.mu = format(num.mu,nsmall = 2)
  str.mu = substr(str.mu,3,nchar(str.mu))
  str.mu = paste("mu",str.mu,sep="")
  num.sigma = params[i,"sigma"]
  str.sigma = format(num.sigma,nsmall = 2)
  str.sigma = substr(str.sigma,3,nchar(str.sigma))
  str.sigma = paste("sd",str.sigma,sep="")
  num.gamma = params[i,"gamma"]
  str.gamma = format(num.gamma/10,nsmall = 3)
  str.gamma = substr(str.gamma,3,nchar(str.gamma))
  str.gamma = paste("g",str.gamma,sep="")
  num.n = params[i,"n"]
  str.n = paste("n",num.n,sep="")
  num.epsilon = params[i,"epsilon"]
  str.epsilon = format(num.epsilon/10,nsmall = 3)
  str.epsilon = substr(str.epsilon,3,nchar(str.epsilon))
  str.epsilon = paste("e",str.epsilon,sep="")
  num.alpha = params[i,"alpha"]
  str.alpha = format(num.alpha/10,nsmall = 3)
  str.alpha = substr(str.alpha,3,nchar(str.alpha))
  str.alpha = paste("a",str.alpha,sep="")
  # tested ok
  #print(paste(i,str.mu,str.sigma,str.gamma,str.n,str.epsilon,str.alpha))
  index.temp = numeric()
  #counter.temp = 0
  # find respective subfolder in output
  for (j in 1:length(list.runs)) {
    run = list.runs[j]
    if (grepl(str.mu,run) & grepl(str.sigma,run) & grepl(str.gamma,run) &
        grepl(str.n,run) & grepl(str.epsilon,run) & grepl(str.alpha,run))
    {
      #counter.temp = counter.temp + 1
      # we didn't get the nserc. just got the news. let's try harder.
      index.temp = j #index.temp[counter.temp] = j
    }
  }
  # list of files in respective output folder
  list.files.temp = list.files(paste(folder.runs,list.runs[index.temp],sep=""),
                               full.names = TRUE)
  
## for each of the 6 algorithms:
  for (algorithm in vec.algorithms) {
### find the output file
    for (k in 1:length(list.files.temp)) {
      file.rewards = list.files.temp[k]
      found = FALSE
      if (grepl(algorithm,file.rewards) & grepl("rewards",file.rewards))
      {
        found = TRUE
        break
      }
    }
    if (found) {rewards = read.csv(file.rewards,sep=" ",header=FALSE)} else {stop("rewards file not found!");}
    rewards = as.matrix(rewards) # now it is converted from char to double

### for each timestep 
#### calculate the average score of all runs (sum/numruns)
### calculate the average of the above-calculated averages (sum/numsteps)
    array.timeSteps[i,,algorithm] = apply(rewards,2,mean)

### yes let's get rid of that, for we are calculating the means now
### write the result in the respective dray (params rownum) of the respective array (algorithm scores)
    mat.scores[i,algorithm] = mean(rewards)
    
## new addition: keeping individual run results
    for (z in 1:nrow(rewards)) {
      array.timeSteps.sim[i,,z,algorithm] = rewards[z,]
    }
  } ## end for algorithm
} # end for row in params

# saving on disk
save(array.timeSteps.sim,file = paste(folder.social,"arrayTimeStepsSim",sep=""))
save(array.timeSteps,file = paste(folder.social,"arrayTimeSteps",sep=""))

# test ok
#(sum(abs(apply(array.timeSteps.sim,c(1,2,4),mean) - array.timeSteps))) # 0 ok

# Alright the scores matrix is now ready. Let's see what we got!
# # test ok
# (dim(mat.scores))
# (typeof(mat.scores))
# (sum(is.na(mat.scores)))
# (dim(array.timeSteps))
# (typeof(array.timeSteps))
# (dim(apply(array.timeSteps,c(1,3),mean)))
# (head(array.timeSteps))
# (head(apply(array.timeSteps,c(1,3),mean)))
# (sum(mat.scores-apply(array.timeSteps,c(1,3),mean))) # tested ok: mat.scores equals average per timestep rewards in array.timeSteps
hist(mat.scores,xlim=c(-1,0))
for (algorithm in vec.algorithms) {
  hist(mat.scores[,algorithm],
       main=paste("Histogram of mean rewards \n",algorithm),
       xlab="Rewards",ylab="Frequency",
       ylim=c(0,as.integer(0.33*nrow(params))),
       xlim=c(-1,0),breaks=20)
}

plot(x=params$mu,y=mat.scores[,1],col=rgb(params$alpha,0,0))
plot(x=params$mu,y=mat.scores[,1],col=rgb(0,params$gamma,0))
plot(x=params$mu,y=mat.scores[,1],col=rgb(0,0,(params$n)/max(params$n)))
# tried various plots. the one parameter with the most effect
# on rewards is mu.
# plotting with various colors shows gamma can have an effect:
# at greater mu, higher rewards have higher gamma.

# next: plotting curves of episodes for select param settings
# requires an array: one mean value per timestep for each row 
#  of params
plot(x=1:ncol(template),
     y=apply(array.timeSteps[,,algorithm],2,mean),
     main=paste("Mean rewards through episodes \n",algorithm),
     xlab="TimeStep",ylab="Rewards",
     ylim=c(-1,0)
     )


plot(x=1:ncol(template),
     type="n",
     main=paste("Mean rewards through episodes"),
     xlab="TimeStep",ylab="Rewards",
     ylim=c(-1,0)
)
for (i in 1:length(vec.algorithms)) {
  algorithm = vec.algorithms[i]
  for (mu in unique(params$mu)) {
    index.mu = which(params$mu == mu)
    points(x=1:ncol(template),
           y=apply(array.timeSteps[index.mu,,algorithm],2,mean),
           col=rgb(mu,0,0),
           pch=i)
  }
}
# best algorithms: 3(TD_DoubleSARSA)
# followed by: 1,2(TD_DoubleExpectedSARSA , TD_DoubleQLearning)
# then: 4(TD_SARSA)
# 5&6 are remarkably lower. 
# at the higher mu, 4&5&6 are lower, 1,2&3 are higher.
# preselect the "Double Learning" algorithms
# let's see the effect of alpha
plot(x=1:ncol(template),type="n",main=paste("Mean rewards through episodes"),xlab="TimeStep",ylab="Rewards",ylim=c(-1,0))
for (i in 1:3) { # algorithms
  algorithm = vec.algorithms[i]
  for (mu in unique(params$mu)) {
    index.mu = which(params$mu == mu)
    points(x=1:ncol(template),
           y=apply(array.timeSteps[index.mu,,algorithm],2,mean),
           col=rgb(mu,0,0),
           pch=i
          )
    # for (alpha in unique(params$alpha)) {
    #   index.alpha = which(params[index.mu,"alpha"]==alpha)
    #   for (r in index.alpha) {
    #     for (timeStep in 1:ncol(template)) {
    #       points(x=timeStep,
    #              y=array.timeSteps[r,timeStep,algorithm],
    #              col=rgb(mu,0,0),
    #              pch=which(unique(params$alpha)==alpha)
    #              )
    #     }
    #   }
    # }
  }
}
# this plot instruction bloc doesn't work. commented out.

unique.mu = unique(params$mu)
ranked.mu = numeric(nrow(params))
for (i in 1:length(unique.mu)) {
  ranked.mu[params$mu == unique.mu[i]] = i
}
rankedVector = function(vec) {
  unique.vec = unique(vec)
  ranked.vec = numeric(length(vec))
  for (i in 1:length(unique.vec)) {
    ranked.vec[vec==unique.vec[i]] = i
  }
  return(ranked.vec)
}
plot(x=1:ncol(template),type="n",main=paste("Mean rewards through episodes \n",algorithm),xlab="TimeStep",ylab="Rewards",ylim=c(-1,0))
for (i in 1:ncol(template)) {
  x.temp = rep(i, nrow(params))
  y.temp = array.timeSteps[,i,algorithm]
  points(x.temp,y.temp,
         pch=ranked.mu,
         col=rgb(params$alpha,0,0))
}
# ok that's better, let's separate plots by mu

ranked.alpha = rankedVector(params$alpha)
plot(x=1:ncol(template),type="n",main=paste("Mean rewards through episodes"),xlab="TimeStep",ylab="Rewards",ylim=c(-1,0))
for (mu in unique(params$mu)) {
  plot(x=1:ncol(template),type="n",main=paste("Mean rewards through episodes \n",algorithm),xlab="TimeStep",ylab="Rewards",ylim=c(-1,0))
  index.mu = which(params$mu == mu)
  for (i in 1:ncol(template)) {
    x.temp = rep(i, length(index.mu))
    y.temp = array.timeSteps[index.mu,i,algorithm]
    points(x.temp,y.temp,
           pch=ranked.alpha,
           col=rgb(params$gamma,0,0))
  }
}

# strictly for this code:
# function to return data averages by parameter values 
averageByParam = function(index.selection=1:nrow(params) , parameter , algorithm) {
  # identify unique parameters values
  unique.params = unique(params[,parameter])
  mat.averages = matrix(data=NA,nrow=length(unique.params),ncol=dim(array.timeSteps)[2])
  for (i in 1:length(unique.params)) {
    p = unique.params[i]
    # select the lines of data whose parameter value are the same
    index.params = which(params[index.selection,parameter] == p)
    # for each selection, calculate an average
    mat.averages[i,] = apply(array.timeSteps[index.params,,algorithm],2,mean)
    # combine the calculated averages
  }
  row.names(mat.averages) = paste(parameter,unique.params)
  # return the combined calculated averages
  return(mat.averages)
}

averageBy.mu = averageByParam(parameter = "mu" , algorithm = algorithm)
# (averageBy.mu) # test ok
# apply(averageBy.mu,1,
#       FUN=function(x) 
#         plot(x,ylim=c(-1,0),xlab="TimeStep",ylab="Rewards",main=row.names(x))
#       )
# for (i in 1:nrow(averageBy.mu)) {
#   plot(x=1:ncol(averageBy.mu),y=averageBy.mu[i,],
#        xlab="TimeStep",ylab="Rewards",
#        main=rownames(averageBy.mu)[i],
#        ylim=c(-1,0))
# }

plotMatrixByRows = function(mat,algorithm,overlay=FALSE) {
  x=1:ncol(mat)
  if (overlay==TRUE) {
    plot(x,type="n",xlab=NULL,ylab=NULL, # wouldn't fit in 6x6 plot
         #main=paste(algorithm,"\n",strsplit(rownames(mat)[1] , " ")[[1]][1]),
         xaxt="n", yaxt="n", ylim=c(-1,0)) # modified to fint in 6x6 plot
    title(paste(algorithm,"\n",strsplit(rownames(mat)[1] , " ")[[1]][1]) , 
          line=0.25 , cex.main=0.9)
  }
  for (i in 1:nrow(mat)) {
    y=mat[i,]
    if (overlay==TRUE) {
      points(x,y,
             col=rgb(i/nrow(mat),0,0))} 
    else {
      plot(x,y,
           xlab="TimeStep",ylab="Rewards",
           main=paste(algorithm,"\n",rownames(mat)[i]),
           ylim=c(-1,0))
    }
  }
}

plotMatrixByRows(averageBy.mu , algorithm=algorithm , overlay=FALSE)
plotMatrixByRows(averageBy.mu , algorithm=algorithm , overlay=TRUE)
# test ok
# let's plot for all algorithms now
vec.params = c("mu","sigma","gamma","n","epsilon","alpha")
for (algorithm in vec.algorithms) {
  for (p in vec.params) {
    averageBy.p = averageByParam(parameter = p , algorithm = algorithm)
    plotMatrixByRows(averageBy.p , algorithm = algorithm , overlay = FALSE)
  }
}

# plotting all in one
oldpar = par()
#dev.new(width=5000 , height=5000)
par(mfrow=c(length(vec.algorithms),length(vec.params)),
    mar=c(.5,1,2,1)) # c(bottom,left,top,right)
for (algorithm in vec.algorithms) {
  for (p in vec.params) {
    averageBy.p = averageByParam(parameter = p , algorithm = algorithm)
    plotMatrixByRows(averageBy.p , algorithm = algorithm , overlay = TRUE)
  }
}
#dev.off()
par(oldpar)
# error: figure margins too large
# made a lot of adjustments and produced 30 plots. finally seems fine.

# next compare results with reference
