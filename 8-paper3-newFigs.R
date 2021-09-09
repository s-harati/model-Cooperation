# Paper3: improving some of the figures using package ggplot
library(ggplot2)
library(scales)
library(signs)

scoreseqticks = seq(-1,0,0.25)
scoreseqlabels = signs(scoreseqticks)


#################
# Reading files #
#################
# For Fogure 2:
file.array.meanscores.ref = "D:/Saeed/2019-PhD-MAIN/400-socialModel/analysis/arrayMeanScoresRef"
load(file.array.meanscores.ref)

# For Figures 3,4,6 (and 7):
folder.social = "D:/Saeed/2019-PhD-MAIN/400-socialModel/analysis/"
file.array.timeSteps = paste(folder.social,"arrayTimeSteps",sep="")
load(file.array.timeSteps) # array 810 x 17 x 6
file.array.sim = paste(folder.social,"arrayTimeStepsSim",sep="")
load(file.array.sim) # array 810 x 17 x 50 x 6
file.array.ref = paste(folder.social,"arrayTimeStepsRef",sep="")
load(file.array.ref) # array 810 x 17 x 50
file.params = paste(folder.social,"paramsPermutations.csv",sep="")
params = read.csv(file.params) # data.frame

vec.algorithms = dimnames(array.timeSteps.sim)[[4]]
num.runs.per.folder = dim(array.timeSteps.sim)[3]

unique.epsilon = unique(params$epsilon)
unique.alpha = unique(params$alpha)
unique.mu = unique(params$mu)
unique.sigma = unique(params$sigma)
unique.n = unique(params$n)
unique.gamma = unique(params$gamma)

folder.runs.sim = "D:/Saeed/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest14bigrun/outputN2vomxN2vom_4k/"
list.runs.sim = list.files(folder.runs.sim,full.names = TRUE) # character array

folder.runs.ref.random = "D:/Saeed/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest15random/output/"
list.runs.ref.random = list.files(folder.runs.ref.random)

folder.runs.ref = "D:/Saeed/2019-PhD-MAIN/400-socialModel/410-cal_val/cooptest15/output/"
list.runs.ref = list.files(folder.runs.ref)

# 6 (and 7)
file.template.thresholds = paste(folder.runs.ref,list.runs.ref[1],"/thresholds.txt",sep="")
template.thresholds = read.csv(file.template.thresholds, sep=" ",header=FALSE)
template.thresholds = as.matrix(template.thresholds)

file.template.ref.random = paste(folder.runs.ref.random,list.runs.ref.random[1],"/ref1random.txt",sep="")
template.ref.random = read.csv(file.template.ref.random, sep=" ", header=FALSE) # 400x17
template.ref.random = as.matrix(template.ref.random)


############
# Figure 2 #
############
my.df.fig3x3 = data.frame(meanscores=numeric(18*50*65536),
                          mu=numeric(18*50*65536),
                          n=numeric(18*50*65536)
                          )
my.index = 1
my.vec.mu = c(0.3 , 0.5 , 0.7)
my.vec.n = c(5 , 9 , 13)
for (i in c(1,2,3 , 7,8,9 , 13,14,15)) {
  my.temp.x = as.vector(array.meanscores.ref[c(i,i+3),,])
  my.df.fig3x3[my.index : (my.index+2*50*65536-1),"meanscores"] = my.temp.x 
  my.df.fig3x3[my.index : (my.index+2*50*65536-1),"mu"] = my.vec.mu[(i%/%6)+1]
  
  my.which.n = i%%3
  if (my.which.n==0) {my.which.n=3}
  my.df.fig3x3[my.index : (my.index+2*50*65536-1),"n"] = my.vec.n[my.which.n]

  my.index = my.index+2*50*65536
}

my.p.fig3x3 = ggplot(data=my.df.fig3x3 , aes(x=meanscores)) + 
  geom_histogram(breaks=seq(-1,0,0.05)) + 
  coord_cartesian(xlim=c(-1,0)) + 
  labs(x = "Score" , y = "Frequency" ,
       title = "Histograms of scores of runs with all sequences of actions by governing agent",
       subtitle = expression(paste("Arranged by number (n) and mean decision threshold (",mu,") of user agents"))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(0.5 , "cm" , data = NULL)) +
  scale_x_continuous(breaks = scoreseqticks,
                     labels = scoreseqlabels) + 
  scale_y_continuous(label= function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", "%*%10^", scientific_format()(x)))))} )

my.p.fig3x3 + 
  facet_grid(rows=vars(mu) , cols=vars(n) , 
             labeller = label_bquote(rows = mu*"="*.(mu),
                                     cols = n*"="*.(n)))
# saved as png: 756x534

#################
# Figures 3 & 4 #
#################
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

## heatmap
### order of rows and columns
chcr <- hclust(dist(t(mat.heatmap.sim.scores)))
rhcr <- hclust(dist(mat.heatmap.sim.scores))

lgd_ = rep(NA, 30)

# Figure 3 #
heatmap(mat.heatmap.sim.scores , main="Simulation scores heatmap",
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none" , col=heat.colors(90))
lgd_[c(1,30)] = c("Max","Min")
legend("right",
       inset = c(0.1,0),
       legend = lgd_,
       fill = rev(heat.colors(30)),
       bty="n",
       border = NA,
       y.intersp = 0.2,
       cex = 0.7, text.font = 2,
       x.intersp = 0.1,
       xpd = TRUE)
# saved as png: 1000x600

# Figure 4 #
heatmap(mat.heatmap.sim.ranks , main="Simulation ranks heatmap",
        Rowv=as.dendrogram(rhcr) , Colv=as.dendrogram(chcr) ,
        scale="none" , col=cm.colors(90))
lgd_[c(1,30)] = c("High","Low")
legend("right",
       inset = c(0.1,0),
       legend = lgd_,
       fill = rev(cm.colors(30)),
       bty="n",
       border = NA,
       y.intersp = 0.2,
       cex = 0.7, text.font = 2,
       x.intersp = 0.1,
       xpd = TRUE)
# saved as png: 1000x600


############
# Figure 6 #
############
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
#save(array.timeSteps.ref.random,file = paste(folder.social,"arrayTimeStepsRefRandom",sep=""))

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



array.17.sim = array(data=NA,
                     dim=c(nrow(params),  # 810
                           num.runs.per.folder, # 50
                           length(vec.algorithms)), # 6
                     dimnames=list(1:nrow(params),
                                   1:num.runs.per.folder,
                                   vec.algorithms)
)

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
my.df.fig6 = data.frame(values=numeric(3*40500),
                        names=vector(length=3*40500))
my.fig6.names = colnames(df.fig7.40500)
my.fig6.names = gsub("\\."," ",my.fig6.names)
for (i in 1:3) {
  my.temp.index = ((i-1)*40500+1):(i*40500)
  my.df.fig6[my.temp.index,"values"] = df.fig7.40500[,i]
  my.df.fig6[my.temp.index,"names"] = my.fig6.names[i]
}
my.df.fig6$names_ord = factor(my.df.fig6$names,
                              levels = c("Double SARSA","Double Expected SARSA","Baseline"))
my.p.fig6.base = ggplot(data = my.df.fig6 , aes(x=values)) + 
  geom_histogram(breaks=seq(-1,0,0.05)) +
  coord_cartesian(xlim=c(-1,0) , ylim=c(0,25000)) +
  labs(x="Score" , y="Frequency" , 
       title = "Histograms of scores of final steps",
       subtitle = "Selected algorithms and a baseline") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(0.5,"cm",data = NULL)) +
  scale_x_continuous(breaks = scoreseqticks,
                     labels = scoreseqlabels) +
  scale_y_continuous(labels = comma)
my.p.fig6 = my.p.fig6.base + facet_grid(cols=vars(names_ord))
my.p.fig6
# saved as png: 705x300


############
# Figure 7 #
############
# Builds on the data prepared for Figure 6
df.fig8 = data.frame("DXSm3"=as.vector(mat.17.sim.DXS[which(params$mu==0.3),]),
                     "DSm3" =as.vector(mat.17.sim.DS[which(params$mu==0.3),]),
                     "basm3"=sample(as.vector(mat.17.ref[1:6,]),13500),
                     #"break1"=rep(NA,13500),
                     #"break11"=rep(NA,13500),
                     "DXSm5"=as.vector(mat.17.sim.DXS[which(params$mu==0.5),]),
                     "DSm5" =as.vector(mat.17.sim.DS[which(params$mu==0.5),]),
                     "basm5"=sample(as.vector(mat.17.ref[7:12,]),13500),
                     #"break2"=rep(NA,13500),
                     #"break22"=rep(NA,13500),
                     "DXSm7"=as.vector(mat.17.sim.DXS[which(params$mu==0.7),]),
                     "DSm7" =as.vector(mat.17.sim.DS[which(params$mu==0.7),]),
                     "basm7"=sample(as.vector(mat.17.ref[13:18,]),13500) )



my.df.fig7 = data.frame(values=numeric(9*13500),
                        names=vector(length=9*13500),
                        mu=numeric(9*13500))
my.fig7.names = c("Double SARSA","Double Expected SARSA","Baseline",
                  "Double SARSA","Double Expected SARSA","Baseline",
                  "Double SARSA","Double Expected SARSA","Baseline")
my.fig7.mu = c(0.3 , 0.3 , 0.3,
                0.5 , 0.5 , 0.5,
                0.7 , 0.7 , 0.7)
for (i in 1:9) {
  my.temp.index = ((i-1)*13500+1):(i*13500)
  my.df.fig7[my.temp.index,"values"] = df.fig8[,i]
  my.df.fig7[my.temp.index,"names"] = my.fig7.names[i]
  my.df.fig7[my.temp.index,"mu"] = my.fig7.mu[i]
}
my.df.fig7$names_ord = factor(my.df.fig7$names,
                              levels = c("Double SARSA","Double Expected SARSA","Baseline"))


my.p.fig7.base = ggplot(data = my.df.fig7 , aes(x=names_ord , y=values)) + 
  stat_boxplot(width = 0.5) +
  coord_cartesian(ylim=c(-1,0)) +
  labs(x = "" , y="Rewards" , 
       title = "Boxplots of scores of final steps",
       subtitle = "Selected algorithms and a baseline") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45 , hjust = 1 , vjust = 1)) +
  scale_y_continuous(breaks = scoreseqticks,
                     labels = scoreseqlabels)
my.p.fig7 = my.p.fig7.base + facet_grid(cols=vars(mu) , labeller = label_bquote(cols = mu*"="*.(mu)))
my.p.fig7


# Edit:
############
# Figure 5 #
############
plot(1:17,type="n",xlim=c(1,17),ylim=c(-1,0),
     yaxt="n",
     xlab="Time steps",ylab="Rewards")
axis(side=2, at=scoreseqticks , labels=scoreseqlabels)
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
       y.intersp=1.0 , x.intersp= 0.1,
       seg.len=1.0)
# saved as png: 800x600