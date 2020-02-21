library('ggplot2')
library('corrplot')
library(tree)
library(partykit)
library(glmnet)
library(e1071)
library(rpart)
library(corrplot)
library(rpart)
library('tidyverse')
# install.packages("keras")
library(keras)
# install_keras()
library(tensorflow)
# install_tensorflow()

source("DataAnalyticsFunctions-A15.R")
#############################################################################################################
### Preparation for running the code
### 1. install Anaconda 3.x, Python 3.x, Jave
### 2. install package 'tensorflow' in cmd using code 'pip install tensorflow' (install package'pip'if u don't have it yet)
### 3. download the DataAnalyticsFunction.R we provided, we change the DataAnalyticsFunction a bit, so please use our version, or there could be error
### 4. It might take an hour or more to run the whole code, please be patient
#############################################################################################################


##############################################################################################################
### Data preparation (visulization and clustering)
#############################################################################################################


###explore and visualize
library("ggplot2")

data <- read.csv("online_shoppers_intention.csv")

head(data)
str(data)
summary(data)

#visitor type - purchase decision
# Revenue <- rep("",length(data$revenue))
# Revenue[data$Revenue== TRUE]    <- "Yes"
# Revenue[data$Revenue== FALSE]    <- "No"
plot(factor(Revenue)~factor(VisitorType), data=data,
     ylab="Purchase or Not",xlab="Visitor Type",
     main="Fig.2 Relationship between Visitor Type and Purchase Decision")

#month - purchase decision
plot(factor(Revenue)~factor(Month), data=data,
     ylab="Purchase or Not",xlab="Month",
     main="Fig.2 Relationship between Month and Purchase Decision")

#The "Page Value" feature represents the average value for a web page that a user visited before completing an e-commerce transaction
a<-ggplot(data,aes(x=as.factor(Revenue),y=PageValues))+
  geom_boxplot()+
  labs(x='Purchase or Not',
       y='Avg Page Value',
       title='Fig.3 Avg Page Value per Purchase Decision')

# compute lower and upper whiskers
ylim2 = boxplot.stats(data$PageValues)$stats[c(1, 5)]


#special - purchase decision
plot(factor(Revenue)~factor(SpecialDay), data=data,
     ylab="Purchase or Not",xlab="Special Day",
     main="Fig.4 Relationship between Special Day and Purchase Decision")
#1 = closest to special day

#Product Related Duration
b <- ggplot(data,aes(x=as.factor(Revenue),y=ProductRelated_Duration))+
  geom_boxplot()+
  labs(x='Purchase or Not',
       y='Product Web Page Duration',
       title='Fig.1 Product Web Page Duration per Purchase Decision')

# compute lower and upper whiskers
ylim1 = boxplot.stats(data$ProductRelated_Duration)$stats[c(1, 5)]

# scale y limits based on ylim1
p1 = b + coord_cartesian(ylim = ylim1*1.05)
p1
#yes stay on a page longer - cut outliers

#Informational_Duration
c <- ggplot(data,aes(x=as.factor(Revenue),y=Informational_Duration))+
  geom_boxplot()+
  labs(x='Purchase or Not',
       y='Informational_Duration',
       title='Fig.6 Informational_Duration per Purchase Decision')
c
# compute lower and upper whiskers
ylim3 = boxplot.stats(data$Informational_Duration)$stats[c(1, 5)]

# scale y limits based on ylim1
c1 = c + coord_cartesian(ylim = ylim1*1.05)
c1

#Administrative_Duration
d <- ggplot(data,aes(x=as.factor(Revenue),y=Informational_Duration))+
  geom_boxplot()+
  labs(x='Purchase or Not',
       y='Administrative_Duration',
       title='Fig.7 Administrative_Duration per Purchase Decision')
d
# compute lower and upper whiskers
ylim4 = boxplot.stats(data$Administrative_Duration)$stats[c(1, 5)]

# scale y limits based on ylim1
d1 = d + coord_cartesian(ylim = ylim1*1.05)
d1

names(data)

################################################
##clustering
for_cluster <- read.csv("online_shoppers_intention.csv")
source("DataAnalyticsFunctions.R")
#drop those that are not meaningful, and outcome variable
drops <- c("Month","VisitorType", "Weekend", "Revenue", "OperatingSystems", "Browser", "Region", "TrafficType", "BounceRates","ExitRates", "PageValues", "SpecialDay")
cordata <- for_cluster[ , !(names(data) %in% drops)]
str(cordata)
names(cordata)

#number of pages and duration are highly correlated
library(corrplot)
corplot <- cor(cordata)
corrplot(corplot, method = "circle")

simple_kmeans <- kmeans(cordata,5,nstart=10)


summary(simple_kmeans
)
colorcluster <- 1+simple_kmeans$cluster

Ssimple <- scale(cordata)

Ssimple_kmeans <- kmeans(Ssimple,5,nstart=10)
colorcluster <- 1+Ssimple_kmeans$cluster

#find mean for revenue and size
Ssimple_kmeans$centers
Ssimple_kmeans$cluster
tapply(data[,18],Ssimple_kmeans$cluster,mean)
Ssimple_kmeans$size
mean(data[,18])
mean(data$Revenue)
tapply(data[,16],Ssimple_kmeans$cluster,table)
colorcluster <- 1+Ssimple_kmeans$cluster

max.size <-5
legend.breaks <- c(1,2,3,4)
legend.r <- 0.5*(legend.breaks/max(data[,7]))*max.size
legend.r <- 2*sqrt(legend.breaks)
legend.loc <- c(70,15000)
legend.cex <- 0.5
symbol.bg <-"white"
symbol.fg <- "black"
MF <- 450


r <- rev(legend.r); b <- rev(legend.breaks)
for (i in 1:length(r)) {
  symbols(legend.loc[1],legend.loc[2]+MF*r[i],
          circle=r[i],inches=FALSE,add=TRUE,
          bg=symbol.bg,fg=symbol.fg)
  lines(c(legend.loc[1],legend.loc[1]+1.2*r[1]),
        rep(legend.loc[2]+2*MF*r[i],2))
  text(legend.loc[1]+1.2*r[1]+par("cxy")[1]/2,
       legend.loc[2]+2*MF*r[i], b[i], adj=c(0,.5),
       cex=legend.cex)
}


kfit <- lapply(1:30, function(k) kmeans(Ssimple,k,nstart=10))

kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")



### computing # of clusters in our example:
kfit <- lapply(1:50, function(k) kmeans(Ssimple,k,nstart=10))
# choose number of clusters based on the fit above
# we will use the  script kIC in DataAnalyticsFunctions.R
# We call the function kIC the performance of the various
# kmeans for k=1,...50, that was stored in kfit.
# Then "A" for AICc (default) or "B" for BIC
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")
## Now we plot them, first we plot AIC
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)",
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)
# Vertical line where AIC is minimized
abline(v=which.min(kaic))
# Next we plot BIC
lines(kbic, col=4, lwd=2)
# Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
# Next we plot HDIC
lines(kHDic, col=3, lwd=2)
# Vertical line where HDIC is minimized
abline(v=which.min(kHDic),col=3)




##############################################################################################################
### Modeling(including three steps)
##### step 1: modeling on original data
##### step 2.1: modeling on oversampling data
##### step 2.2: modeling on undersampling data
##### step 3: Feature selection
#############################################################################################################

#############################################################################################################
### START of MODELING
### STEP 1: MODELING on ORIGINAL DATA
#############################################################################################################

### load the data
data<-read.csv('online_shoppers_intention.csv')
set.seed(1)

### cleaning data
nrow(data)
sum(complete.cases(data))

data$OperatingSystems<-as.factor(data$OperatingSystems)
data$Browser<-as.factor(data$Browser)
data$Region<-as.factor(data$Region)
data$TrafficType<-as.factor(data$TrafficType)

cor_matrix<-cor(data[,1:10])
corrplot(cor_matrix)

data2<-data[,c(-1,-3,-5,-7,-15,-13,-12)]

newdata<-data2

### setting holdout sample
set.seed(1)
holdout.indices <- sample(nrow(newdata), nrow(data2)*0.25)
data.holdout <- newdata[holdout.indices,]
newdata <- newdata[-holdout.indices,]
nrow(newdata)
nrow(data.holdout)


###Just checking how balanced they are:
mean(newdata$Revenue=="YES")
mean(data.holdout$Revenue=="YES")


nfold <- 10
n <- nrow(newdata)
set.seed(1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

### This defined the features we will use the matrix Mx (X) and the target My (Y)

Mx<- model.matrix(Revenue ~ .^2, newdata)[,-1]
My<- newdata$Revenue == "YES"

Mx.holdout<- model.matrix(Revenue ~ .^2, data.holdout)[,-1]
My.holdout<- data.holdout$Revenue == "YES"

#### Lasso requires a penalty parameter lambda

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lassoTheory <- glmnet(Mx,My,family="binomial",lambda = lambda.theory)
summary(lassoTheory)

### Preparation for lasso
lasso<-glmnet(Mx,My,family = 'binomial')
lassoCV <- cv.glmnet(Mx,My, family="binomial")
lassoCV$lambda[which.min(lassoCV$cvm)]


features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
features.theory <- support(lassoTheory$beta)


newdata.min <- data.frame(Mx[,features.min],My)
newdata.1se <- data.frame(Mx[,features.1se],My)
newdata.theory <- data.frame(Mx[,features.theory],My)

newdata.min.holdout <- data.frame(Mx.holdout[,features.min],My.holdout)
newdata.1se.holdout <- data.frame(Mx.holdout[,features.1se],My.holdout)
newdata.theory.holdout <- data.frame(Mx.holdout[,features.theory],My.holdout)



OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold)) 

OOS.TPR<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

OOS.FPR<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

OOS.ACC<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

### choose 0.5 as threhold
val=.5

### calculation for all the models
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  model.logistic.interaction <-glm(Revenue~.^2, data=newdata, subset=train, family="binomial")
  model.logistic <-glm(Revenue~., data=newdata, subset=train,family="binomial")
  model.tree <- rpart(Revenue~ ., data=newdata, subset=train,method='class',control=rpart.control(minsplit=60, cp=0.005)) 
  model.null <-glm(Revenue~1, data=newdata, subset=train,family="binomial")
  ## get predictions: type=response so we have probabilities
  pred.logistic.interaction <- predict(model.logistic.interaction, newdata=newdata[-train,], type="response")
  pred.logistic             <- predict(model.logistic, newdata=newdata[-train,], type="response")
  pred.tree                 <- predict(model.tree, newdata=newdata[-train,], type="prob")
  pred.tree <- pred.tree[,2]
  pred.null <- predict(model.null, newdata=newdata[-train,], type="response")
  
  ## calculate R2
  # Logistic Interaction
  OOS$logistic.interaction[k] <- R2(y=newdata$Revenue[-train], pred=pred.logistic.interaction, family="binomial")
  OOS$logistic.interaction[k]
  # Logistic
  OOS$logistic[k] <- R2(y=newdata$Revenue[-train], pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  # Tree
  OOS$tree[k] <- R2(y=newdata$Revenue[-train], pred=pred.tree, family="binomial")
  OOS$tree[k]
  #Null
  OOS$null[k] <- R2(y=newdata$Revenue[-train], pred=pred.null, family="binomial")
  OOS$null[k]
  
  ### postlasso-min
  rmin <- glm(My~., data=newdata.min, subset=train, family="binomial")
  ### pl-1se
  if ( length(features.1se) == 0){  r1se <- glm(Revenue~1, data=newdata, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=newdata.1se, subset=train, family="binomial")
  }
  ### pl-theory
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Revenue~1, data=newdata, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=newdata.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=newdata.min[-train,], type="response")
  pred1se  <- predict(r1se,newdata=newdata.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=newdata.theory[-train,], type="response")
  
  
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="binomial")
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se, family="binomial")
  OOS$PL.theory[k] <- R2(y=My[-train], pred=predtheory, family="binomial")
  
  
  ### L-min
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  ## L-1se
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  ### L-theory
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="binomial")
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="binomial")
  OOS$L.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="binomial")
  
  #average model
  predaverage<-(pred.logistic + pred.logistic.interaction + pred.tree + predmin + pred1se+ predtheory + predlassomin + predlasso1se + predlassotheory)/9
  OOS$average[k] <- R2(y=newdata$Revenue[-train],pred=predaverage,family='binomial')
  
  #cv of ACC,TPR, FPR
  values<-FPR_TPR((predlassomin>=val),My[-train])
  OOS.ACC$L.min[k]<-values$ACC
  OOS.TPR$L.min[k]<-values$TPR
  OOS.FPR$L.min[k]<-values$FPR
  
  
  values<-FPR_TPR((predlasso1se>=val),My[-train])
  OOS.ACC$L.1se[k]<-values$ACC
  OOS.TPR$L.1se[k]<-values$TPR
  OOS.FPR$L.1se[k]<-values$FPR
  
  
  values<-FPR_TPR((predlassotheory>=val),My[-train])
  OOS.ACC$L.theory[k]<-values$ACC
  OOS.TPR$L.theory[k]<-values$TPR
  OOS.FPR$L.theory[k]<-values$FPR
  
  
  values<-FPR_TPR((predmin>=val),My[-train])
  OOS.ACC$PL.min[k]<-values$ACC
  OOS.TPR$PL.min[k]<-values$TPR
  OOS.FPR$PL.min[k]<-values$FPR
  
  
  values<-FPR_TPR((pred1se>=val),My[-train])
  OOS.ACC$PL.1se[k]<-values$ACC
  OOS.TPR$PL.1se[k]<-values$TPR
  OOS.FPR$PL.1se[k]<-values$FPR
  
  
  values<-FPR_TPR((predtheory>=val),My[-train])
  OOS.ACC$PL.theory[k]<-values$ACC
  OOS.TPR$PL.theory[k]<-values$TPR
  OOS.FPR$PL.theory[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.tree>=val),My[-train])
  OOS.ACC$tree[k]<-values$ACC
  OOS.TPR$tree[k]<-values$TPR
  OOS.FPR$tree[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.logistic>=val),My[-train])
  OOS.ACC$logistic[k]<-values$ACC
  OOS.TPR$logistic[k]<-values$TPR
  OOS.FPR$logistic[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.logistic.interaction>=val),My[-train])
  OOS.ACC$logistic.interaction[k]<-values$ACC
  OOS.TPR$logistic.interaction[k]<-values$TPR
  OOS.FPR$logistic.interaction[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.null>=val),My[-train])
  OOS.ACC$null[k]<-values$ACC
  OOS.TPR$null[k]<-values$TPR
  OOS.FPR$null[k]<-values$FPR
  
  
  values<-FPR_TPR((predaverage>=val),My[-train])
  OOS.ACC$average[k]<-values$ACC
  OOS.TPR$average[k]<-values$TPR
  OOS.FPR$average[k]<-values$FPR
  
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

### plotting the CV for all models

names(OOS)[8] <-"logistic\ninteraction"
names(OOS.ACC)[8] <-"logistic\ninteraction"
names(OOS.TPR)[8] <-"logistic\ninteraction"
names(OOS.FPR)[8] <-"logistic\ninteraction"

barplot(colMeans(OOS), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
OOS

barplot(colMeans(OOS.ACC), las=2, ylim=c(min(colMeans(OOS.ACC))*0.975,max(colMeans(OOS.ACC))+0.1),xpd=FALSE, xlab="", ylab = "Accuracy")

barplot(colMeans(OOS.TPR), las=2, ylim=c(0.35,1),xpd=FALSE, xlab="", ylab = "TPR")

barplot(colMeans(OOS.FPR), las=2,xpd=FALSE, xlab="", ylab = "FPR")
names(OOS)[8] <-"logistic.interaction"
names(OOS.ACC)[8] <-"logistic.interaction"
names(OOS.TPR)[8] <-"logistic.interaction"
names(OOS.FPR)[8] <-"logistic.interaction"
print(model.tree)



### calculate models' performance in holdout sample

holdout <- data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.TPR<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.FPR<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.ACC<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.F1<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

#####R2 of models in holdout sample
### logistic, interaction, classificaiton tree and null
pred.logistic.interaction <- predict(model.logistic.interaction, newdata=data.holdout, type="response")
pred.logistic             <- predict(model.logistic, newdata=data.holdout, type="response")
pred.tree                 <- predict(model.tree, newdata=data.holdout, type="prob")
pred.tree <- pred.tree[,2]
pred.null <- predict(model.null, newdata=data.holdout, type="response")


holdout$logistic.interaction <- R2(y=data.holdout$Revenue, pred=pred.logistic.interaction, family="binomial")
holdout$logistic <- R2(y=data.holdout$Revenue, pred=pred.logistic, family="binomial")
holdout$tree <- R2(y=data.holdout$Revenue, pred=pred.tree, family="binomial")
holdout$null <- R2(y=data.holdout$Revenue, pred=pred.null, family="binomial")


### post lasso andlasso
predmin <- predict(rmin, newdata=newdata.min.holdout, type="response")
pred1se  <- predict(r1se,newdata=newdata.1se.holdout, type="response")
predtheory <- predict(rtheory, newdata=newdata.theory.holdout, type="response")
holdout$PL.min <- R2(y=My.holdout, pred=predmin, family="binomial")
holdout$PL.1se <- R2(y=My.holdout, pred=pred1se, family="binomial")
holdout$PL.theory <- R2(y=My.holdout, pred=predtheory, family="binomial")

predlassomin <- predict(lassomin, newx=Mx.holdout, type="response")
predlasso1se  <- predict(lasso1se, newx=Mx.holdout, type="response")
predlassotheory <- predict(lassoTheory, newx=Mx.holdout, type="response")
holdout$L.min <- R2(y=My.holdout, pred=predlassomin, family="binomial")
holdout$L.1se <- R2(y=My.holdout, pred=predlasso1se, family="binomial")
holdout$L.theory <- R2(y=My.holdout, pred=predlassotheory, family="binomial")

#average model
predaverage<-(pred.logistic + pred.tree + predmin + pred1se+ predtheory + predlassomin + predlasso1se + predlassotheory)/9
holdout$average <- R2(y=data.holdout$Revenue,pred=predaverage,family='binomial')

#### ACC, FPR, TPR, and F1 of samples in holdout sample

values<-FPR_TPR((predlassomin>=val),My.holdout)
holdout.ACC$L.min<-values$ACC
holdout.TPR$L.min<-values$TPR
holdout.FPR$L.min<-values$FPR
holdout.F1$L.min<-values$F1

values<-FPR_TPR((predlasso1se>=val),My.holdout)
holdout.ACC$L.1se<-values$ACC
holdout.TPR$L.1se<-values$TPR
holdout.FPR$L.1se<-values$FPR
holdout.F1$L.1se<-values$F1

values<-FPR_TPR((predlassotheory>=val),My.holdout)
holdout.ACC$L.theory<-values$ACC
holdout.TPR$L.theory<-values$TPR
holdout.FPR$L.theory<-values$FPR
holdout.F1$L.theory<-values$F1

values<-FPR_TPR((predmin>=val),My.holdout)
holdout.ACC$PL.min<-values$ACC
holdout.TPR$PL.min<-values$TPR
holdout.FPR$PL.min<-values$FPR
holdout.F1$PL.min<-values$F1

values<-FPR_TPR((pred1se>=val),My.holdout)
holdout.ACC$PL.1se<-values$ACC
holdout.TPR$PL.1se<-values$TPR
holdout.FPR$PL.1se<-values$FPR
holdout.F1$PL.1se<-values$F1

values<-FPR_TPR((predtheory>=val),My.holdout)
holdout.ACC$PL.theory<-values$ACC
holdout.TPR$PL.theory<-values$TPR
holdout.FPR$PL.theory<-values$FPR
holdout.F1$PL.theory<-values$F1

values<-FPR_TPR((pred.tree>=val),My.holdout)
holdout.ACC$tree<-values$ACC
holdout.TPR$tree<-values$TPR
holdout.FPR$tree<-values$FPR
holdout.F1$tree<-values$F1

values<-FPR_TPR((pred.logistic>=val),My.holdout)
holdout.ACC$logistic<-values$ACC
holdout.TPR$logistic<-values$TPR
holdout.FPR$logistic<-values$FPR
holdout.F1$logistic<-values$F1

values<-FPR_TPR((pred.logistic.interaction>=val),My.holdout)
holdout.ACC$logistic.interaction<-values$ACC
holdout.TPR$logistic.interaction<-values$TPR
holdout.FPR$logistic.interaction<-values$FPR
holdout.F1$logistic.interaction<-values$F1

values<-FPR_TPR((pred.null>=val),My.holdout)
holdout.ACC$null<-values$ACC
holdout.TPR$null<-values$TPR
holdout.FPR$null<-values$FPR
holdout.F1$null<-values$F1

values<-FPR_TPR((predaverage>=val),My.holdout)
holdout.ACC$average<-values$ACC
holdout.TPR$average<-values$TPR
holdout.FPR$average<-values$FPR
holdout.F1$average<-values$F1



###########DNN start




x.holdout<- model.matrix(Revenue ~ .^2, data=data.holdout)[,-1]
y.holdout<- data.holdout$Revenue == "YES"

x.data<- model.matrix(Revenue ~ .^2., data=newdata)[,-1]
y.data<- newdata$Revenue == "YES"

#rescale (to be between 0 and 1)
x_train <- x.data %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_train <- as.numeric(y.data)

x_test <- x.holdout %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_test <- as.numeric(y.holdout) 

#rescale (unit variance and zero mean)
mean <- apply(x.data,2,mean)
std <- apply(x.data,2,sd)
x_train <- scale(x.data,center = mean, scale = std)[,features.theory]
y_train <- as.numeric(y.data)

mean2 <- apply(x.holdout,2,mean)
std2 <- apply(x.holdout,2,sd)
x_test <- scale(x.holdout,center = mean2, scale = std2)[,features.theory]
y_test <- as.numeric(y.holdout) 
num.inputs <- ncol(x_train)

# models start
model <- keras_model_sequential() %>%
  layer_dense(units=512, kernel_regularizer = regularizer_l2(0.013), activation="relu",input_shape = c(num.inputs)) %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=512, kernel_regularizer = regularizer_l2(0.013), activation="relu") %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=256, kernel_regularizer = regularizer_l2(0.013), activation="relu") %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=128, kernel_regularizer = regularizer_l2(0.013), activation="relu") %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=1,activation="sigmoid")


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 50, batch_size = 512, 
  validation_split = 0.3
)

#evaluate the performance of DNN in holdout sample
pred_DNN<-predict(model,x=x_test)
R2(pred=pred_DNN,y=y_test,family='binomial')
values<-FPR_TPR((pred_DNN>=val),y_test)
values

holdout$DNN<-R2(pred=pred_DNN,y=y_test,family='binomial')
holdout.ACC$DNN<-values$ACC
holdout.TPR$DNN<-values$TPR
holdout.FPR$DNN<-values$FPR
holdout.F1$DNN<-values$F1
colMeans(holdout)
colMeans(holdout.ACC)
colMeans(holdout.TPR)
colMeans(holdout.FPR)

### plot the OOS preformance for all models

names(holdout)[8] <-"logistic\ninteraction"
names(holdout.ACC)[8] <-"logistic\ninteraction"
names(holdout.TPR)[8] <-"logistic\ninteraction"
names(holdout.FPR)[8] <-"logistic\ninteraction"

barplot(colMeans(holdout), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ R^2),ylim=c(-0.1,0.4))
OOS

barplot(colMeans(holdout.ACC), las=2, ylim=c(min(colMeans(holdout.ACC))*0.975,max(colMeans(holdout.ACC))),xpd=FALSE, xlab="", ylab = "Accuracy")

barplot(colMeans(holdout.TPR), las=2,xpd=FALSE, xlab="", ylab = "TPR")

barplot(colMeans(holdout.FPR), las=2,xpd=FALSE, xlab="", ylab = "FPR")

names(OOS)[8] <-"logistic.interaction"
names(OOS.ACC)[8] <-"logistic.interaction"
names(OOS.TPR)[8] <-"logistic.interaction"
names(OOS.FPR)[8] <-"logistic.interaction"
print(model.tree)

holdout
holdout.ACC
holdout.TPR
holdout.FPR
holdout.F1

#############################################################################################################
### STEP 2.1:MODELING on UNDERSAMPLING DATA
### The following part is the copy of the last part, except that they are running on different data
#############################################################################################################
data<-read.csv('online_shoppers_intention.csv')

nrow(data)
sum(complete.cases(data))

data$OperatingSystems<-as.factor(data$OperatingSystems)
data$Browser<-as.factor(data$Browser)
data$Region<-as.factor(data$Region)
data$TrafficType<-as.factor(data$TrafficType)


cor_matrix<-cor(data[,1:10])
corrplot(cor_matrix)

data2<-data[,c(-1,-3,-5,-7,-15,-13,-12)]
set.seed(1)
holdout.indices <- sample(nrow(data2), 3000)
data.holdout <- data2[holdout.indices,]
newdata <- data2[-holdout.indices,]

data_NO<-newdata[newdata$Revenue == "NO",]
data_YES<-newdata[newdata$Revenue == "YES",]
set.seed(1)
sample_NO_indice<-sample(nrow(data_NO),nrow(data_YES))
sample_NO<-data_NO[sample_NO_indice,]
newdata<-rbind(sample_NO,data_YES)
set.seed(1)
newdata<-newdata[sample(nrow(newdata)),]

nrow(newdata)
nrow(data.holdout)





###Just checking how balanced they are:
mean(newdata$Revenue=="YES")
mean(data.holdout$Revenue=="YES")


nfold <- 10
n <- nrow(newdata)
set.seed(1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

Mx<- model.matrix(Revenue ~ .^2, newdata)[,-1]
My<- newdata$Revenue == "YES"

Mx.holdout<- model.matrix(Revenue ~ .^2, data.holdout)[,-1]
My.holdout<- data.holdout$Revenue == "YES"


### This defined the features we will use the matrix Mx (X) and the target My (Y)
###
#### Lasso requires a penalty parameter lambda

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lassoTheory <- glmnet(Mx,My,family="binomial",lambda = lambda.theory)
summary(lassoTheory)

lasso<-glmnet(Mx,My,family = 'binomial')
lassoCV <- cv.glmnet(Mx,My, family="binomial")
lassoCV$lambda[which.min(lassoCV$cvm)]


features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory)

newdata.min <- data.frame(Mx[,features.min],My)
newdata.1se <- data.frame(Mx[,features.1se],My)
newdata.theory <- data.frame(Mx[,features.theory],My)

newdata.min.holdout <- data.frame(Mx.holdout[,features.min],My.holdout)
newdata.1se.holdout <- data.frame(Mx.holdout[,features.1se],My.holdout)
newdata.theory.holdout <- data.frame(Mx.holdout[,features.theory],My.holdout)



OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold)) 

OOS.TPR<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

OOS.FPR<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

OOS.ACC<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

val=.5

for(k in 1:nfold){
  train <- which(foldid!=k) # train on all but fold `k'
  
  model.logistic.interaction <-glm(Revenue~.^2, data=newdata, subset=train, family="binomial")
  model.logistic <-glm(Revenue~., data=newdata, subset=train,family="binomial")
  model.tree <- rpart(Revenue~ ., data=newdata, subset=train,method='class',control=rpart.control(minsplit=60, cp=0.005))
  model.null <-glm(Revenue~1, data=newdata, subset=train,family="binomial")
  ## get predictions: type=response so we have probabilities
  pred.logistic.interaction <- predict(model.logistic.interaction, newdata=newdata[-train,], type="response")
  pred.logistic             <- predict(model.logistic, newdata=newdata[-train,], type="response")
  pred.tree                 <- predict(model.tree, newdata=newdata[-train,], type="prob")
  pred.tree <- pred.tree[,2]
  pred.null <- predict(model.null, newdata=newdata[-train,], type="response")
  
  ## calculate and log R2
  # Logistic Interaction
  OOS$logistic.interaction[k] <- R2(y=newdata$Revenue[-train], pred=pred.logistic.interaction, family="binomial")
  OOS$logistic.interaction[k]
  # Logistic
  OOS$logistic[k] <- R2(y=newdata$Revenue[-train], pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  # Tree
  OOS$tree[k] <- R2(y=newdata$Revenue[-train], pred=pred.tree, family="binomial")
  OOS$tree[k]
  #Null
  OOS$null[k] <- R2(y=newdata$Revenue[-train], pred=pred.null, family="binomial")
  OOS$null[k]
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=newdata.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Revenue~1, data=newdata, subset=train, family="binomial")
  } else {r1se <- glm(My~., data=newdata.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){
    rtheory <- glm(Revenue~1, data=newdata, subset=train, family="binomial")
  } else {rtheory <- glm(My~., data=newdata.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=newdata.min[-train,], type="response")
  pred1se  <- predict(r1se,newdata=newdata.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=newdata.theory[-train,], type="response")
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="binomial")
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se, family="binomial")
  OOS$PL.theory[k] <- R2(y=My[-train], pred=predtheory, family="binomial")
  
  
  
  ### This is the CV for the Lasso estimates
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="binomial")
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="binomial")
  OOS$L.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="binomial")
  
  predaverage<-(pred.logistic + pred.logistic.interaction + pred.tree + predmin + pred1se+ predtheory + predlassomin + predlasso1se + predlassotheory)/9
  OOS$average[k] <- R2(y=newdata$Revenue[-train],pred=predaverage,family='binomial')
  
  values<-FPR_TPR((predlassomin>=val),My[-train])
  OOS.ACC$L.min[k]<-values$ACC
  OOS.TPR$L.min[k]<-values$TPR
  OOS.FPR$L.min[k]<-values$FPR
  
  
  values<-FPR_TPR((predlasso1se>=val),My[-train])
  OOS.ACC$L.1se[k]<-values$ACC
  OOS.TPR$L.1se[k]<-values$TPR
  OOS.FPR$L.1se[k]<-values$FPR
  
  
  values<-FPR_TPR((predlassotheory>=val),My[-train])
  OOS.ACC$L.theory[k]<-values$ACC
  OOS.TPR$L.theory[k]<-values$TPR
  OOS.FPR$L.theory[k]<-values$FPR
  
  
  values<-FPR_TPR((predmin>=val),My[-train])
  OOS.ACC$PL.min[k]<-values$ACC
  OOS.TPR$PL.min[k]<-values$TPR
  OOS.FPR$PL.min[k]<-values$FPR
  
  
  values<-FPR_TPR((pred1se>=val),My[-train])
  OOS.ACC$PL.1se[k]<-values$ACC
  OOS.TPR$PL.1se[k]<-values$TPR
  OOS.FPR$PL.1se[k]<-values$FPR
  
  
  values<-FPR_TPR((predtheory>=val),My[-train])
  OOS.ACC$PL.theory[k]<-values$ACC
  OOS.TPR$PL.theory[k]<-values$TPR
  OOS.FPR$PL.theory[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.tree>=val),My[-train])
  OOS.ACC$tree[k]<-values$ACC
  OOS.TPR$tree[k]<-values$TPR
  OOS.FPR$tree[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.logistic>=val),My[-train])
  OOS.ACC$logistic[k]<-values$ACC
  OOS.TPR$logistic[k]<-values$TPR
  OOS.FPR$logistic[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.logistic.interaction>=val),My[-train])
  OOS.ACC$logistic.interaction[k]<-values$ACC
  OOS.TPR$logistic.interaction[k]<-values$TPR
  OOS.FPR$logistic.interaction[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.null>=val),My[-train])
  OOS.ACC$null[k]<-values$ACC
  OOS.TPR$null[k]<-values$TPR
  OOS.FPR$null[k]<-values$FPR
  
  
  values<-FPR_TPR((predaverage>=val),My[-train])
  OOS.ACC$average[k]<-values$ACC
  OOS.TPR$average[k]<-values$TPR
  OOS.FPR$average[k]<-values$FPR
  
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}



names(OOS)[8] <-"logistic\ninteraction"
names(OOS.ACC)[8] <-"logistic\ninteraction"
names(OOS.TPR)[8] <-"logistic\ninteraction"
names(OOS.FPR)[8] <-"logistic\ninteraction"

barplot(colMeans(OOS), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
OOS

barplot(colMeans(OOS.ACC), las=2, ylim=c(min(colMeans(OOS.ACC))*0.975,max(colMeans(OOS.ACC))+0.1),xpd=FALSE, xlab="", ylab = "Accuracy")

barplot(colMeans(OOS.TPR), las=2, ylim=c(0.35,1),xpd=FALSE, xlab="", ylab = "TPR")

barplot(colMeans(OOS.FPR), las=2,xpd=FALSE, xlab="", ylab = "FPR")
names(OOS)[8] <-"logistic.interaction"
names(OOS.ACC)[8] <-"logistic.interaction"
names(OOS.TPR)[8] <-"logistic.interaction"
names(OOS.FPR)[8] <-"logistic.interaction"
print(model.tree)





holdout <- data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.TPR<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.FPR<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.ACC<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.F1<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 


pred.logistic.interaction <- predict(model.logistic.interaction, newdata=data.holdout, type="response")
pred.logistic             <- predict(model.logistic, newdata=data.holdout, type="response")
pred.tree                 <- predict(model.tree, newdata=data.holdout, type="prob")
pred.tree <- pred.tree[,2]
pred.null <- predict(model.null, newdata=data.holdout, type="response")

## calculate and log R2
# Logistic Interaction
holdout$logistic.interaction <- R2(y=data.holdout$Revenue, pred=pred.logistic.interaction, family="binomial")
# Logistic
holdout$logistic <- R2(y=data.holdout$Revenue, pred=pred.logistic, family="binomial")
# Tree
holdout$tree <- R2(y=data.holdout$Revenue, pred=pred.tree, family="binomial")
#Null
holdout$null <- R2(y=data.holdout$Revenue, pred=pred.null, family="binomial")



predmin <- predict(rmin, newdata=newdata.min.holdout, type="response")
pred1se  <- predict(r1se,newdata=newdata.1se.holdout, type="response")
predtheory <- predict(rtheory, newdata=newdata.theory.holdout, type="response")
holdout$PL.min <- R2(y=My.holdout, pred=predmin, family="binomial")
holdout$PL.1se <- R2(y=My.holdout, pred=pred1se, family="binomial")
holdout$PL.theory <- R2(y=My.holdout, pred=predtheory, family="binomial")




predlassomin <- predict(lassomin, newx=Mx.holdout, type="response")
predlasso1se  <- predict(lasso1se, newx=Mx.holdout, type="response")
predlassotheory <- predict(lassoTheory, newx=Mx.holdout, type="response")
holdout$L.min <- R2(y=My.holdout, pred=predlassomin, family="binomial")
holdout$L.1se <- R2(y=My.holdout, pred=predlasso1se, family="binomial")
holdout$L.theory <- R2(y=My.holdout, pred=predlassotheory, family="binomial")

predaverage<-(pred.logistic + pred.tree + predmin + pred1se+ predtheory + predlassomin + predlasso1se + predlassotheory)/9
holdout$average <- R2(y=data.holdout$Revenue,pred=predaverage,family='binomial')



values<-FPR_TPR((predlassomin>=val),My.holdout)
holdout.ACC$L.min<-values$ACC
holdout.TPR$L.min<-values$TPR
holdout.FPR$L.min<-values$FPR
holdout.F1$L.min<-values$F1

values<-FPR_TPR((predlasso1se>=val),My.holdout)
holdout.ACC$L.1se<-values$ACC
holdout.TPR$L.1se<-values$TPR
holdout.FPR$L.1se<-values$FPR
holdout.F1$L.1se<-values$F1

values<-FPR_TPR((predlassotheory>=val),My.holdout)
holdout.ACC$L.theory<-values$ACC
holdout.TPR$L.theory<-values$TPR
holdout.FPR$L.theory<-values$FPR
holdout.F1$L.theory<-values$F1

values<-FPR_TPR((predmin>=val),My.holdout)
holdout.ACC$PL.min<-values$ACC
holdout.TPR$PL.min<-values$TPR
holdout.FPR$PL.min<-values$FPR
holdout.F1$PL.min<-values$F1

values<-FPR_TPR((pred1se>=val),My.holdout)
holdout.ACC$PL.1se<-values$ACC
holdout.TPR$PL.1se<-values$TPR
holdout.FPR$PL.1se<-values$FPR
holdout.F1$PL.1se<-values$F1

values<-FPR_TPR((predtheory>=val),My.holdout)
holdout.ACC$PL.theory<-values$ACC
holdout.TPR$PL.theory<-values$TPR
holdout.FPR$PL.theory<-values$FPR
holdout.F1$PL.theory<-values$F1

values<-FPR_TPR((pred.tree>=val),My.holdout)
holdout.ACC$tree<-values$ACC
holdout.TPR$tree<-values$TPR
holdout.FPR$tree<-values$FPR
holdout.F1$tree<-values$F1

values<-FPR_TPR((pred.logistic>=val),My.holdout)
holdout.ACC$logistic<-values$ACC
holdout.TPR$logistic<-values$TPR
holdout.FPR$logistic<-values$FPR
holdout.F1$logistic<-values$F1

values<-FPR_TPR((pred.logistic.interaction>=val),My.holdout)
holdout.ACC$logistic.interaction<-values$ACC
holdout.TPR$logistic.interaction<-values$TPR
holdout.FPR$logistic.interaction<-values$FPR
holdout.F1$logistic.interaction<-values$F1

values<-FPR_TPR((pred.null>=val),My.holdout)
holdout.ACC$null<-values$ACC
holdout.TPR$null<-values$TPR
holdout.FPR$null<-values$FPR
holdout.F1$null<-values$F1

values<-FPR_TPR((predaverage>=val),My.holdout)
holdout.ACC$average<-values$ACC
holdout.TPR$average<-values$TPR
holdout.FPR$average<-values$FPR
holdout.F1$average<-values$F1



###########DNN start



# install.packages("keras")
# library(keras)
# install_keras()
# library(tensorflow)
# install_tensorflow()

x.holdout<- model.matrix(Revenue ~ .^2, data=data.holdout)[,-1]
y.holdout<- data.holdout$Revenue == "YES"


x.data<- model.matrix(Revenue ~ .^2., data=newdata)[,-1]
y.data<- newdata$Revenue == "YES"

#rescale (to be between 0 and 1)
x_train <- x.data %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_train <- as.numeric(y.data)

x_test <- x.holdout %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_test <- as.numeric(y.holdout) 

#rescale (unit variance and zero mean)
mean <- apply(x.data,2,mean)
std <- apply(x.data,2,sd)
x_train <- scale(x.data,center = mean, scale = std)[,features.theory]
y_train <- as.numeric(y.data)

mean2 <- apply(x.holdout,2,mean)
std2 <- apply(x.holdout,2,sd)
x_test <- scale(x.holdout,center = mean2, scale = std2)[,features.theory]
y_test <- as.numeric(y.holdout) 

F1<-data.frame(drop=rep(NA,1000),units=rep(NA,1000),regular=rep(NA,1000),F1=rep(NA,1000))
num.inputs <- ncol(x_train)

model <- keras_model_sequential() %>%
  layer_dense(units=300, kernel_regularizer = regularizer_l2(0.06), activation="relu",input_shape = c(num.inputs)) %>%
  layer_dropout(rate=0.12) %>%
  layer_dense(units=300, kernel_regularizer = regularizer_l2(0.06), activation="relu") %>%
  layer_dropout(rate=0.12) %>%
  layer_dense(units=300, kernel_regularizer = regularizer_l2(0.06), activation="relu") %>%
  layer_dropout(rate=0.12) %>%
  layer_dense(units=250, kernel_regularizer = regularizer_l2(0.06), activation="relu") %>%
  layer_dropout(rate=0.12) %>%
  layer_dense(units=1,activation="sigmoid")


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 500, 
  validation_split = 0.30
)
# results.NN4 <- model %>% evaluate(x_train,y_train)
# results.NN4
# 
# results.NN4 <- model %>% evaluate(x_test,y_test)
# results.NN4

pred_DNN<-predict(model,x=x_test)
R2(pred=pred_DNN,y=y_test,family='binomial')
values<-FPR_TPR((pred_DNN>=val),y_test)
values

na.omit(F1[F1$F1>0.4,])

holdout$DNN<-R2(pred=pred_DNN,y=y_test,family='binomial')
holdout.ACC$DNN<-values$ACC
holdout.TPR$DNN<-values$TPR
holdout.FPR$DNN<-values$FPR
holdout.F1$DNN<-values$F1
colMeans(holdout)
colMeans(holdout.ACC)
colMeans(holdout.TPR)
colMeans(holdout.FPR)
colMeans(holdout.F1)



names(holdout)[8] <-"logistic\ninteraction"
names(holdout.ACC)[8] <-"logistic\ninteraction"
names(holdout.TPR)[8] <-"logistic\ninteraction"
names(holdout.FPR)[8] <-"logistic\ninteraction"

barplot(colMeans(holdout), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ R^2),ylim=c(-0.1,0.2))
OOS

barplot(colMeans(holdout.ACC), las=2, ylim=c(min(colMeans(holdout.ACC))*0.975,max(colMeans(holdout.ACC))),xpd=FALSE, xlab="", ylab = "Accuracy")

barplot(colMeans(holdout.TPR), las=2, ylim=c(0.35,1),xpd=FALSE, xlab="", ylab = "TPR")

barplot(colMeans(holdout.FPR), las=2,xpd=FALSE, xlab="", ylab = "FPR")
barplot(colMeans(holdout.F1), las=2,xpd=FALSE, xlab="", ylab = "F1",ylim=c(0,0.9))
names(OOS)[8] <-"logistic.interaction"
names(OOS.ACC)[8] <-"logistic.interaction"
names(OOS.TPR)[8] <-"logistic.interaction"
names(OOS.FPR)[8] <-"logistic.interaction"
print(model.tree)

holdout
holdout.ACC
holdout.TPR
holdout.FPR
holdout.F1

#############################################################################################################
### STEP 2.2: MODELING via OVERSAMPLING
#############################################################################################################
data<-read.csv('online_shoppers_intention.csv')

nrow(data)
sum(complete.cases(data))

data$OperatingSystems<-as.factor(data$OperatingSystems)
data$Browser<-as.factor(data$Browser)
data$Region<-as.factor(data$Region)
data$TrafficType<-as.factor(data$TrafficType)



cor_matrix<-cor(data[,1:10])
corrplot(cor_matrix)

data2<-data[,c(-1,-3,-5,-7,-15,-13,-12)]
set.seed(1)
holdout.indices <- sample(nrow(data2), 3000)
data.holdout <- data2[holdout.indices,]
newdata <- data2[-holdout.indices,]

data_NO<-newdata[newdata$Revenue == "NO",]
data_YES<-newdata[newdata$Revenue == "YES",]

set.seed(1)
sample_YES<-sample_n(data_YES,nrow(data_NO),replace=TRUE)

newdata<-rbind(sample_YES,data_NO)
set.seed(1)
newdata<-newdata[sample(nrow(newdata)),]

nrow(newdata)
nrow(data.holdout)





###Just checking how balanced they are:
mean(newdata$Revenue=="YES")
mean(data.holdout$Revenue=="YES")


nfold <- 10
n <- nrow(newdata)
set.seed(1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

Mx<- model.matrix(Revenue ~ .^2, newdata)[,-1]
My<- newdata$Revenue == "YES"

Mx.holdout<- model.matrix(Revenue ~ .^2, data.holdout)[,-1]
My.holdout<- data.holdout$Revenue == "YES"


### This defined the features we will use the matrix Mx (X) and the target My (Y)
###
#### Lasso requires a penalty parameter lambda

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is

lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lassoTheory <- glmnet(Mx,My,family="binomial",lambda = lambda.theory)
summary(lassoTheory)

lasso<-glmnet(Mx,My,family = 'binomial')
lassoCV <- cv.glmnet(Mx,My, family="binomial")
lassoCV$lambda[which.min(lassoCV$cvm)]


features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 


features.theory <- support(lassoTheory$beta)
length(features.theory)

newdata.min <- data.frame(Mx[,features.min],My)
newdata.1se <- data.frame(Mx[,features.1se],My)
newdata.theory <- data.frame(Mx[,features.theory],My)

newdata.min.holdout <- data.frame(Mx.holdout[,features.min],My.holdout)
newdata.1se.holdout <- data.frame(Mx.holdout[,features.1se],My.holdout)
newdata.theory.holdout <- data.frame(Mx.holdout[,features.theory],My.holdout)



OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold)) 

OOS.TPR<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

OOS.FPR<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

OOS.ACC<-data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),logistic=rep(NA,nfold),logistic.interaction=rep(NA,nfold),tree=rep(NA,nfold),average=rep(NA,nfold),null=rep(NA,nfold))

val=.5

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  model.logistic.interaction <-glm(Revenue~.^2, data=newdata, subset=train, family="binomial")
  model.logistic <-glm(Revenue~., data=newdata, subset=train,family="binomial")
  model.tree <- rpart(Revenue~ ., data=newdata, subset=train,method='class',control=rpart.control(minsplit=60, cp=0.005)) 
  model.null <-glm(Revenue~1, data=newdata, subset=train,family="binomial")
  ## get predictions: type=response so we have probabilities
  pred.logistic.interaction <- predict(model.logistic.interaction, newdata=newdata[-train,], type="response")
  pred.logistic             <- predict(model.logistic, newdata=newdata[-train,], type="response")
  pred.tree                 <- predict(model.tree, newdata=newdata[-train,], type="prob")
  pred.tree <- pred.tree[,2]
  pred.null <- predict(model.null, newdata=newdata[-train,], type="response")
  
  ## calculate and log R2
  # Logistic Interaction
  OOS$logistic.interaction[k] <- R2(y=newdata$Revenue[-train], pred=pred.logistic.interaction, family="binomial")
  OOS$logistic.interaction[k]
  # Logistic
  OOS$logistic[k] <- R2(y=newdata$Revenue[-train], pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  # Tree
  OOS$tree[k] <- R2(y=newdata$Revenue[-train], pred=pred.tree, family="binomial")
  OOS$tree[k]
  #Null
  OOS$null[k] <- R2(y=newdata$Revenue[-train], pred=pred.null, family="binomial")
  OOS$null[k]
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=newdata.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Revenue~1, data=newdata, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=newdata.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Revenue~1, data=newdata, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=newdata.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=newdata.min[-train,], type="response")
  pred1se  <- predict(r1se,newdata=newdata.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=newdata.theory[-train,], type="response")
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="binomial")
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se, family="binomial")
  OOS$PL.theory[k] <- R2(y=My[-train], pred=predtheory, family="binomial")
  
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="binomial")
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="binomial")
  OOS$L.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="binomial")
  

  
  predaverage<-(pred.logistic + pred.logistic.interaction + pred.tree + predmin + pred1se+ predtheory + predlassomin + predlasso1se + predlassotheory)/9
  OOS$average[k] <- R2(y=newdata$Revenue[-train],pred=predaverage,family='binomial')
  
  values<-FPR_TPR((predlassomin>=val),My[-train])
  OOS.ACC$L.min[k]<-values$ACC
  OOS.TPR$L.min[k]<-values$TPR
  OOS.FPR$L.min[k]<-values$FPR
  
  
  values<-FPR_TPR((predlasso1se>=val),My[-train])
  OOS.ACC$L.1se[k]<-values$ACC
  OOS.TPR$L.1se[k]<-values$TPR
  OOS.FPR$L.1se[k]<-values$FPR
  
  
  values<-FPR_TPR((predlassotheory>=val),My[-train])
  OOS.ACC$L.theory[k]<-values$ACC
  OOS.TPR$L.theory[k]<-values$TPR
  OOS.FPR$L.theory[k]<-values$FPR
  
  
  values<-FPR_TPR((predmin>=val),My[-train])
  OOS.ACC$PL.min[k]<-values$ACC
  OOS.TPR$PL.min[k]<-values$TPR
  OOS.FPR$PL.min[k]<-values$FPR
  
  
  values<-FPR_TPR((pred1se>=val),My[-train])
  OOS.ACC$PL.1se[k]<-values$ACC
  OOS.TPR$PL.1se[k]<-values$TPR
  OOS.FPR$PL.1se[k]<-values$FPR
  
  
  values<-FPR_TPR((predtheory>=val),My[-train])
  OOS.ACC$PL.theory[k]<-values$ACC
  OOS.TPR$PL.theory[k]<-values$TPR
  OOS.FPR$PL.theory[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.tree>=val),My[-train])
  OOS.ACC$tree[k]<-values$ACC
  OOS.TPR$tree[k]<-values$TPR
  OOS.FPR$tree[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.logistic>=val),My[-train])
  OOS.ACC$logistic[k]<-values$ACC
  OOS.TPR$logistic[k]<-values$TPR
  OOS.FPR$logistic[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.logistic.interaction>=val),My[-train])
  OOS.ACC$logistic.interaction[k]<-values$ACC
  OOS.TPR$logistic.interaction[k]<-values$TPR
  OOS.FPR$logistic.interaction[k]<-values$FPR
  
  
  values<-FPR_TPR((pred.null>=val),My[-train])
  OOS.ACC$null[k]<-values$ACC
  OOS.TPR$null[k]<-values$TPR
  OOS.FPR$null[k]<-values$FPR
  
  
  values<-FPR_TPR((predaverage>=val),My[-train])
  OOS.ACC$average[k]<-values$ACC
  OOS.TPR$average[k]<-values$TPR
  OOS.FPR$average[k]<-values$FPR
  
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}



names(OOS)[8] <-"logistic\ninteraction"
names(OOS.ACC)[8] <-"logistic\ninteraction"
names(OOS.TPR)[8] <-"logistic\ninteraction"
names(OOS.FPR)[8] <-"logistic\ninteraction"

barplot(colMeans(OOS), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
OOS

barplot(colMeans(OOS.ACC), las=2, ylim=c(min(colMeans(OOS.ACC))*0.975,max(colMeans(OOS.ACC))+0.1),xpd=FALSE, xlab="", ylab = "Accuracy")

barplot(colMeans(OOS.TPR), las=2, ylim=c(0.35,1),xpd=FALSE, xlab="", ylab = "TPR")

barplot(colMeans(OOS.FPR), las=2,xpd=FALSE, xlab="", ylab = "FPR")
names(OOS)[8] <-"logistic.interaction"
names(OOS.ACC)[8] <-"logistic.interaction"
names(OOS.TPR)[8] <-"logistic.interaction"
names(OOS.FPR)[8] <-"logistic.interaction"
print(model.tree)





holdout <- data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.TPR<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.FPR<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.ACC<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 

holdout.F1<-data.frame(L.min=NA, L.1se=NA, L.theory=NA, PL.min=NA, PL.1se=NA, PL.theory=NA,logistic=NA,logistic.interaction=NA,tree=NA,average=NA,DNN=NA,null=NA) 


pred.logistic.interaction <- predict(model.logistic.interaction, newdata=data.holdout, type="response")
pred.logistic             <- predict(model.logistic, newdata=data.holdout, type="response")
pred.tree                 <- predict(model.tree, newdata=data.holdout, type="prob")
pred.tree <- pred.tree[,2]
pred.null <- predict(model.null, newdata=data.holdout, type="response")

## calculate and log R2
# Logistic Interaction
holdout$logistic.interaction <- R2(y=data.holdout$Revenue, pred=pred.logistic.interaction, family="binomial")
# Logistic
holdout$logistic <- R2(y=data.holdout$Revenue, pred=pred.logistic, family="binomial")
# Tree
holdout$tree <- R2(y=data.holdout$Revenue, pred=pred.tree, family="binomial")
#Null
holdout$null <- R2(y=data.holdout$Revenue, pred=pred.null, family="binomial")




predmin <- predict(rmin, newdata=newdata.min.holdout, type="response")
pred1se  <- predict(r1se,newdata=newdata.1se.holdout, type="response")
predtheory <- predict(rtheory, newdata=newdata.theory.holdout, type="response")
holdout$PL.min <- R2(y=My.holdout, pred=predmin, family="binomial")
holdout$PL.1se <- R2(y=My.holdout, pred=pred1se, family="binomial")
holdout$PL.theory <- R2(y=My.holdout, pred=predtheory, family="binomial")




predlassomin <- predict(lassomin, newx=Mx.holdout, type="response")
predlasso1se  <- predict(lasso1se, newx=Mx.holdout, type="response")
predlassotheory <- predict(lassoTheory, newx=Mx.holdout, type="response")
holdout$L.min <- R2(y=My.holdout, pred=predlassomin, family="binomial")
holdout$L.1se <- R2(y=My.holdout, pred=predlasso1se, family="binomial")
holdout$L.theory <- R2(y=My.holdout, pred=predlassotheory, family="binomial")

predaverage<-(pred.logistic + pred.tree + predmin + pred1se+ predtheory + predlassomin + predlasso1se + predlassotheory)/9
holdout$average <- R2(y=data.holdout$Revenue,pred=predaverage,family='binomial')



values<-FPR_TPR((predlassomin>=val),My.holdout)
holdout.ACC$L.min<-values$ACC
holdout.TPR$L.min<-values$TPR
holdout.FPR$L.min<-values$FPR
holdout.F1$L.min<-values$F1

values<-FPR_TPR((predlasso1se>=val),My.holdout)
holdout.ACC$L.1se<-values$ACC
holdout.TPR$L.1se<-values$TPR
holdout.FPR$L.1se<-values$FPR
holdout.F1$L.1se<-values$F1

values<-FPR_TPR((predlassotheory>=val),My.holdout)
holdout.ACC$L.theory<-values$ACC
holdout.TPR$L.theory<-values$TPR
holdout.FPR$L.theory<-values$FPR
holdout.F1$L.theory<-values$F1

values<-FPR_TPR((predmin>=val),My.holdout)
holdout.ACC$PL.min<-values$ACC
holdout.TPR$PL.min<-values$TPR
holdout.FPR$PL.min<-values$FPR
holdout.F1$PL.min<-values$F1

values<-FPR_TPR((pred1se>=val),My.holdout)
holdout.ACC$PL.1se<-values$ACC
holdout.TPR$PL.1se<-values$TPR
holdout.FPR$PL.1se<-values$FPR
holdout.F1$PL.1se<-values$F1

values<-FPR_TPR((predtheory>=val),My.holdout)
holdout.ACC$PL.theory<-values$ACC
holdout.TPR$PL.theory<-values$TPR
holdout.FPR$PL.theory<-values$FPR
holdout.F1$PL.theory<-values$F1

values<-FPR_TPR((pred.tree>=val),My.holdout)
holdout.ACC$tree<-values$ACC
holdout.TPR$tree<-values$TPR
holdout.FPR$tree<-values$FPR
holdout.F1$tree<-values$F1

values<-FPR_TPR((pred.logistic>=val),My.holdout)
holdout.ACC$logistic<-values$ACC
holdout.TPR$logistic<-values$TPR
holdout.FPR$logistic<-values$FPR
holdout.F1$logistic<-values$F1

values<-FPR_TPR((pred.logistic.interaction>=val),My.holdout)
holdout.ACC$logistic.interaction<-values$ACC
holdout.TPR$logistic.interaction<-values$TPR
holdout.FPR$logistic.interaction<-values$FPR
holdout.F1$logistic.interaction<-values$F1

values<-FPR_TPR((pred.null>=val),My.holdout)
holdout.ACC$null<-values$ACC
holdout.TPR$null<-values$TPR
holdout.FPR$null<-values$FPR
holdout.F1$null<-values$F1

values<-FPR_TPR((predaverage>=val),My.holdout)
holdout.ACC$average<-values$ACC
holdout.TPR$average<-values$TPR
holdout.FPR$average<-values$FPR
holdout.F1$average<-values$F1



###########DNN start



# install.packages("keras")
library(keras)
# install_keras()
library(tensorflow)
# install_tensorflow()

x.holdout<- model.matrix(Revenue ~ .^2, data=data.holdout)[,-1]
y.holdout<- data.holdout$Revenue == "YES"


x.data<- model.matrix(Revenue ~ .^2., data=newdata)[,-1]
y.data<- newdata$Revenue == "YES"

#rescale (to be between 0 and 1)
x_train <- x.data %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_train <- as.numeric(y.data)

x_test <- x.holdout %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_test <- as.numeric(y.holdout) 

#rescale (unit variance and zero mean)
mean <- apply(x.data,2,mean)
std <- apply(x.data,2,sd)
x_train <- scale(x.data,center = mean, scale = std)[,features.theory]
y_train <- as.numeric(y.data)

mean2 <- apply(x.holdout,2,mean)
std2 <- apply(x.holdout,2,sd)
x_test <- scale(x.holdout,center = mean2, scale = std2)[,features.theory]
y_test <- as.numeric(y.holdout) 

F1<-data.frame(drop=rep(NA,1000),units=rep(NA,1000),regular=rep(NA,1000),F1=rep(NA,1000))
num.inputs <- ncol(x_train)
k=1

model <- keras_model_sequential() %>%
  layer_dense(units=512, kernel_regularizer = regularizer_l2(0.02), activation="relu",input_shape = c(num.inputs)) %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=512, kernel_regularizer = regularizer_l2(0.02), activation="relu") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=256, kernel_regularizer = regularizer_l2(0.02), activation="sigmoid") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=1,activation="sigmoid")


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 512, 
  validation_split = 0.3
)
# results.NN4 <- model %>% evaluate(x_train,y_train)
# results.NN4
# 
# results.NN4 <- model %>% evaluate(x_test,y_test)
# results.NN4

pred_DNN<-predict(model,x=x_test)
R2(pred=pred_DNN,y=y_test,family='binomial')
values<-FPR_TPR((pred_DNN>=val),y_test)
values

print(paste('drop=',drop*0.01,'values=',values$F1))
F1$drop[k]=drop*0.04
F1$F1[k]=values$F1
k=k+1



holdout$DNN<-R2(pred=pred_DNN,y=y_test,family='binomial')
holdout.ACC$DNN<-values$ACC
holdout.TPR$DNN<-values$TPR
holdout.FPR$DNN<-values$FPR
holdout.F1$DNN<-values$F1
colMeans(holdout)
colMeans(holdout.ACC)
colMeans(holdout.TPR)
colMeans(holdout.FPR)




names(holdout)[8] <-"logistic\ninteraction"
names(holdout.ACC)[8] <-"logistic\ninteraction"
names(holdout.TPR)[8] <-"logistic\ninteraction"
names(holdout.FPR)[8] <-"logistic\ninteraction"

barplot(colMeans(holdout), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ R^2),ylim=c(-0.1,0.2))
OOS

barplot(colMeans(holdout.ACC), las=2, ylim=c(min(colMeans(holdout.ACC))*0.975,max(colMeans(holdout.ACC))),xpd=FALSE, xlab="", ylab = "Accuracy")

barplot(colMeans(holdout.TPR), las=2, ylim=c(0.35,1),xpd=FALSE, xlab="", ylab = "TPR")

barplot(colMeans(holdout.FPR), las=2,xpd=FALSE, xlab="", ylab = "FPR")
barplot(colMeans(holdout.F1), las=2,xpd=FALSE, xlab="", ylab = "F1",ylim=c(0,0.9))
names(OOS)[8] <-"logistic.interaction"
names(OOS.ACC)[8] <-"logistic.interaction"
names(OOS.TPR)[8] <-"logistic.interaction"
names(OOS.FPR)[8] <-"logistic.interaction"
print(model.tree)

holdout
holdout.ACC
holdout.TPR
holdout.FPR
holdout.F1

#############################################################################################################
### step 3: feature selection
#############################################################################################################
data <- read.csv("online_shoppers_intention.csv")

## feature selection using information gains using the fselector package
install.packages("FSelector")
library(FSelector)
weight <- information.gain(Revenue~., data)
print(weight)
weight$attr_importance[sort.list(weight$attr_importance,decreasing = TRUE)]

##feature selection using 
install.packages("mRMRe")
library(mRMRe)
set.thread.count(2)



data[[18]] <- as.numeric(data[[18]])
data[[11]] <- ordered(data[[11]])
data[[16]] <- ordered(data[[16]])
data[[12]] <- ordered(data[[12]])
data[[13]] <- ordered(data[[13]])
data[[14]] <- ordered(data[[14]])
data[[15]] <- ordered(data[[15]])
data$Weekend <- as.numeric(data$Weekend)
data$Revenue <- as.numeric(data$Revenue)
data$Administrative <- as.numeric(data$Administrative)
data$Informational <- as.numeric(data$Informational)
data$ProductRelated <- as.numeric(data$ProductRelated)
str(data)
f_data <- mRMR.data(data = data.frame(data))
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 18,
                        feature_count = 10)
solutions(results)
featureNames

#### Using the features selected, we run the classification tree based on undersampling data

data<-read.csv('online_shoppers_intention.csv')

nrow(data)
sum(complete.cases(data))

data$OperatingSystems<-as.factor(data$OperatingSystems)
data$Browser<-as.factor(data$Browser)
data$Region<-as.factor(data$Region)
data$TrafficType<-as.factor(data$TrafficType)


data2<-data[,c(9,8,6,7,5,18)]
set.seed(1)
holdout.indices <- sample(nrow(data2), 3000)
data.holdout <- data2[holdout.indices,]
newdata <- data2[-holdout.indices,]

data_NO<-newdata[newdata$Revenue == "NO",]
data_YES<-newdata[newdata$Revenue == "YES",]
set.seed(1)
sample_NO_indice<-sample(nrow(data_NO),nrow(data_YES))
sample_NO<-data_NO[sample_NO_indice,]
newdata<-rbind(sample_NO,data_YES)
set.seed(1)
newdata<-newdata[sample(nrow(newdata)),]

nrow(newdata)
nrow(data.holdout)

###Just checking how balanced they are:
mean(newdata$Revenue=="YES")
mean(data.holdout$Revenue=="YES")

### predicting using our best model- classification tree
model.tree <- rpart(Revenue~ ., data=newdata,method='class',control=rpart.control(minsplit=60, cp=0.005))
val=0.5
Mx.holdout<- model.matrix(Revenue ~ .^2, data.holdout)[,-1]
My.holdout<- data.holdout$Revenue == "YES"
pred.tree                 <- predict(model.tree, newdata=data.holdout, type="prob")
pred.tree <- pred.tree[,2]
source("DataAnalyticsFunctions.R")
R2 <- R2(y=data.holdout$Revenue, pred=pred.tree, family="binomial")
values<-FPR_TPR((pred.tree>=val),My.holdout)
R2
values
print(model.tree)

######Prediction via L.theory model(not used at last)
# 
# Mx<- model.matrix(Revenue ~ .^2, newdata)[,-1]
# My<- newdata$Revenue == "YES"
# 
# Mx.holdout<- model.matrix(Revenue ~ .^2, data.holdout)[,-1]
# My.holdout<- data.holdout$Revenue == "YES"
# 
# 
# num.features <- ncol(Mx)
# num.n <- nrow(Mx)
# num.churn <- sum(My)
# w <- (num.churn/num.n)*(1-(num.churn/num.n))
# lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
# lassoTheory <- glmnet(Mx,My,family="binomial",lambda = lambda.theory)
# summary(lassoTheory)
# features.theory <- support(lassoTheory$beta)
# length(features.theory)
# newdata.theory <- data.frame(Mx[,features.theory],My)
# newdata.theory.holdout <- data.frame(Mx.holdout[,features.theory],My.holdout)
# lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
# 
# predlassotheory <- predict(lassoTheory, newx=Mx.holdout, type="response")
# 
# source("DataAnalyticsFunctions.R")
# R2<-R2(y=My.holdout, pred=predlassotheory, family="binomial")
# values<-FPR_TPR((predlassotheory>=val),My.holdout)
# 
# R2
# values

#########################################################
###final model
#########################################################

data<-read.csv('online_shoppers_intention.csv')

nrow(data)
sum(complete.cases(data))

data$OperatingSystems<-as.factor(data$OperatingSystems)
data$Browser<-as.factor(data$Browser)
data$Region<-as.factor(data$Region)
data$TrafficType<-as.factor(data$TrafficType)


data2<-data[,c(-1,-3,-5,-7,-15,-13,-12)]
data_NO<-data2[data2$Revenue == "NO",]
data_YES<-data2[data2$Revenue == "YES",]
set.seed(1)
sample_NO_indice<-sample(nrow(data_NO),nrow(data_YES))
sample_NO<-data_NO[sample_NO_indice,]
newdata<-rbind(sample_NO,data_YES)
set.seed(1)
newdata<-newdata[sample(nrow(newdata)),]
model.tree <- rpart(Revenue~ ., data=newdata,method='class',control=rpart.control(minsplit=60, cp=0.005))
print(model.tree)