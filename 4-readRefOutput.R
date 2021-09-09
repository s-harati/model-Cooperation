# code to read reference outputs
# this code generates timeStep scores for each of the 50 runs per parameter setting
# recall that there are 18 parameter settings

# load file of all codes
folder.social = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/analysis/"
file.params = paste(folder.social,"paramsPermutations.csv",sep="")
params = read.csv(file.params) # data.frame
vec.params.ref = c("mu","sigma","n")
unique.mu = unique(params$mu)
unique.sigma = unique(params$sigma)
unique.n = unique(params$n)

# path of reference runs
folder.runs.ref = "C:/Users/p1095659/Documents/UniWorks/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest15/output/"
list.runs.ref = list.files(folder.runs.ref)

file.template.ref = paste(folder.runs.ref,list.runs.ref[1],"/ref1.txt",sep="")
#template.ref = read.csv(file.template.ref, sep=" ",header=FALSE)
template.ref = scan(file.template.ref, sep=" ", what=double())
#template.ref = as.matrix(template.ref) # this is actually one row only
# reminder: file ref1.txt includes average per timestep scores for 2^17 runs
# reminder: the last entry is NA and must be removed
template.ref = template.ref[-length(template.ref)] # last entry removed, now it is a vector and no longer a matrix

file.template.ref.full = paste(folder.runs.ref,list.runs.ref[1],"/ref1full.txt",sep="")
template.ref.full = read.csv(file.template.ref.full, sep=" ",header=FALSE)
template.ref.full = as.matrix(template.ref.full)
template.ref.full = template.ref.full[,-ncol(template.ref.full)] # to remove extra column that contains "\n" only
template.ref.full[is.na(template.ref.full)] = 0
plot(x=1:ncol(template.ref.full) , y=apply(template.ref.full,2,mean))
boxplot(template.ref.full)
file.template.thresholds = paste(folder.runs.ref,list.runs.ref[1],"/thresholds.txt",sep="")
template.thresholds = read.csv(file.template.thresholds, sep=" ",header=FALSE)
template.thresholds = as.matrix(template.thresholds)
#nfiles = nrow(template.thresholds) # 50 files

# create an array, including a matrix for each run folder,
# in each matrix, a row for each of the 50 runs
# on each row, 17 entries for the 17 times steps, 
#  which are calculated as column mean of respective ref*full.text
array.timeSteps.ref = array(data=NA,
                            dim=c(
                              nrow(params), # 810
                              ncol(template.ref.full), #17
                              nrow(template.thresholds)), # 50
                            dimnames=list(
                              1:nrow(params),
                              1:ncol(template.ref.full),
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
      for (j in 1:length(list.runs.ref)) {
        run = list.runs.ref[j]
        if (grepl(str.mu,run) & grepl(str.sigma,run) & grepl(str.n,run))
        {
          index.temp = j 
          break
        }
      }
      # list of files in respective output folder
      list.files.temp.ref = list.files(paste(folder.runs.ref,list.runs.ref[index.temp],sep=""),
                                   full.names = TRUE)
      
      # all params rows corresponding to this ref run
      index.params = which(params$mu==mu & params$sigma==sigma & params$n==n) 
      
      for (i in 1:dim(array.timeSteps.ref)[3]) # each 'page' in the 3d array is associated with a run
      {
        index.rewards.ref = which(grepl(paste("ref",i,"full",sep=""),list.files.temp.ref))
        file.rewards.ref = list.files.temp.ref[index.rewards.ref]
        rewards.ref = read.csv(file.rewards.ref,sep=" ",header=FALSE)
        rewards.ref = as.matrix(rewards.ref) # now it is converted from char to double
        rewards.ref = rewards.ref[,-ncol(rewards.ref)] # to remove extra column that contains "\n" only
        rewards.ref[is.na(rewards.ref)] = 0
        # writing in array.timeSteps.ref
        for (k in index.params) {
          array.timeSteps.ref[k,,i] = colMeans(rewards.ref)
        }
      }
    }
  }
}
save(array.timeSteps.ref,file = paste(folder.social,"arrayTimeStepsRef",sep=""))
#load(paste(folder.social,"arrayTimeStepsRef",sep=""))

# next: find the difference between reference and simulations
# next: divide data points into success and failure, and
#       try to identify a combination of algorithm, epsilon and alpha
#       which produce the highest scores
# next: as Roberto wanted, draw box plots (find nicer visuals) to give 
#       an idea of dispersion

