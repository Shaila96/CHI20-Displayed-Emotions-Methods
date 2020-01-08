### This is the code used for the CHI-2020 paper on the dual task where we examine
### the two groups (Batch versus Continuous) email interruption of their work.
### The recorded response are seven emotions, where the algorithm provides
### the percent of certainty that the face has a particular emotion. These are:
### {1=Angry, 2=Disgusted, 3=Afraid, 4=Happy, 5=Sad, 6=Surprised, 7=Neutral}
### Here we will read the data form the co-occurence matrices


# install.packages("car")
# install.packages("MASS")
# install.packages("grDevices")
# install.packages("gplots")
# install.packages("agricolae")

library(car)
library(MASS)


script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
current_dir <- dirname(script_dir)

source(file.path(script_dir, 'us-common-functions.R'))



os <- get_os()

# setwd("put_the_path_where_the_data_are") 


TFEb<-matrix(NA, 13 ,7) ### Total Facial Emotions for Batch (7 emotion in # of frames)
TFEc<-matrix(NA, 13,7)	### Total Facial Emotions for Continuous (7 emotion in # of frames)

Sb<-c("T016","T021","T051","T077","T079","T083","T085","T106","T139","T144","T166","T175","T178")
Sc<-c("T005","T063","T064","T068","T092","T094","T098","T099","T121","T124","T132","T151","T157")


setwd(file.path(current_dir, 'curated-data', 'Subj Data', 'Batch', 'DT'))
tmpc<-1
for (i in Sb){
	tmpB1<-read.csv(paste(i,".csv",sep=""),header=T,sep=",")
	TFEb[tmpc,1:7]<-c(tmpB1[1,2],tmpB1[2,3],tmpB1[3,4],tmpB1[4,5],tmpB1[5,6],tmpB1[6,7],tmpB1[7,8])
	tmpc<-tmpc+1
}

setwd(file.path(current_dir, 'curated-data', 'Subj Data', 'Continual', 'DT'))
tmpc<-1
for (i in Sc){
	tmpB1<-read.csv(paste(i,".csv",sep=""),header=T,sep=",")
	TFEc[tmpc,1:7]<-c(tmpB1[1,2],tmpB1[2,3],tmpB1[3,4],tmpB1[4,5],tmpB1[5,6],tmpB1[6,7],tmpB1[7,8])
	tmpc<-tmpc+1
}


TB<-cbind(c(TFEb[,1:7]),rep(1:7,each=13),rep(0,7*13))
TC<-cbind(c(TFEc[,1:7]),rep(1:7,each=13),rep(1,7*13))

TFE<-rbind(TB,TC)
TFEper<-rbind(colSums(TFEb[,1:7]), colSums(TFEc[,1:7]))

Em<-TFE[,2]
Em[Em==1]<-"An"
Em[Em==2]<-"Di"
Em[Em==3]<-"Af"
Em[Em==4]<-"Ha"
Em[Em==5]<-"Sa"
Em[Em==6]<-"Su"
Em[Em==7]<-"Ne"
G<-TFE[,3]
G[G==0]<-"B"
G[G==1]<-"C"
# quartz()
# windows()
new_display(os)
interaction.plot(G,Em,TFE[,1], type="b", col=c("orange","red","brown","green","black","blue","cyan"), leg.bty="o", fixed=F,lwd=2, pch=c(18,24,22,17,15,16,19), trace.label="Emotions",xlab="",ylab="")


fit0<-aov(TFE[,1]~ factor (TFE[,2])* factor(TFE[,3]))
fit0
summary(fit0)	### anova table

### Diagnostic Plots
# quartz()
# windows()
new_display(os)
layout(matrix(1:4,2,2))
plot(fit0)

### add a small number in the response to unstack from zero an check the box-cox transformation
fit0<-aov((TFE[,1]+1)~ factor (TFE[,2])* factor(TFE[,3]))
fit0
summary(fit0)	### anova table
### BOX-COX
# quartz()
# windows()
new_display(os)
boxcox(fit0)

fit<-aov(log(TFE[,1]+1)~ factor (TFE[,2])* factor(TFE[,3]))
fit
summary(fit)	### anova table

### Test the normality assumption in the residuals
# quartz()
# windows()
new_display(os)
qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit$residuals)


### Test the assumption of homogeneity of variances
bartlett.test(log(TFE[,1]+1)~factor(TFE[,2]))
bartlett.test(log(TFE[,1])~factor(TFE[,3]))
fligner.test(log(TFE[,1])~factor(TFE[,2]))
fligner.test(log(TFE[,1])~factor(TFE[,3]))


### Diagnostic Plots
# quartz()
# windows()
new_display(os)
layout(matrix(1:4,2,2))
plot(fit)



fit1<-aov(log(TFE[,1]+1)~ factor (TFE[,2])+factor(TFE[,3]))
fit1
summary(fit1)	### anova table


### Test the normality assumption in the residuals
# quartz()
# windows()
new_display(os)
qqnorm(fit1$residuals,main="NPP for residuals")
qqline(fit1$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit1$residuals)


### Test the assumption of homogeneity of variances
bartlett.test(log(TFE[,1])~factor(TFE[,2]))
bartlett.test(log(TFE[,1])~factor(TFE[,3]))
fligner.test(log(TFE[,1])~factor(TFE[,2]))
fligner.test(log(TFE[,1])~factor(TFE[,3]))


### Diagnostic Plots
# quartz()
# windows()
new_display(os)
layout(matrix(1:4,2,2))
plot(fit1)



#########################################
### MULTIPLE COMPARISONS ad-hoc to ANOVA
#########################################

### Error Plot (CI of means)
#############################

fit1<-aov(log(TFE[,1]+1)~ factor(TFE[,2])+factor(TFE[,3]))
fit1
summary(fit1)	### anova table

# Plot Means with Error Bars
# quartz()
# windows()
new_display(os)
par(mfrow=c(1,2))
library(gplots)
plotmeans(log(TFE[,1]+1)~factor(TFE[,2]),xlab="", ylab="", main="Mean plot with 95% CI")
plotmeans(log(TFE[,1]+1)~factor(TFE[,3]),xlab="", ylab="", main="Mean plot with 95% CI")

### Tukey HSD method
TukeyHSD(fit1)


### p-value adjusted pairwise t-tests
pairwise.t.test(log(TFE[,1]+1),factor(TFE[,2]),p.adjust.method="bonferroni")
pairwise.t.test(log(TFE[,1]+1),factor(TFE[,3]),p.adjust.method="bonferroni")
pairwise.t.test(log(TFE[,1]+1),factor(TFE[,2]),p.adjust.method="holm")
pairwise.t.test(log(TFE[,1]+1),factor(TFE[,3]),p.adjust.method="holm")


###Least Significant Differences Method
#install.packages("agricolae", dependencies=TRUE)
DFE<-fit1$df.residual
MSE<-deviance(fit1)/DFE
library(agricolae)
print(LSD.test(log(TFE[,1]+1),factor(TFE[,2]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni"))
LSD.test(log(TFE[,1]+1),factor(TFE[,2]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni")$groups
print(LSD.test(log(TFE[,1]+1),factor(TFE[,3]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni"))
LSD.test(log(TFE[,1]+1),factor(TFE[,3]),DFerror=DFE,MSerror=MSE,p.adj="bonferroni")$groups

### Scheffe's Method
DFE<-fit1$df.residual
MSE<-deviance(fit1)/DFE
library(agricolae)
print(scheffe.test(log(TFE[,1]+1),factor(TFE[,2]), DFerror=DFE, MSerror=MSE))
scheffe.test(log(TFE[,1]+1),factor(TFE[,2]), DFerror=DFE, MSerror=MSE)$groups
print(scheffe.test(log(TFE[,1]+1),factor(TFE[,3]), DFerror=DFE, MSerror=MSE))
scheffe.test(log(TFE[,1]+1),factor(TFE[,3]), DFerror=DFE, MSerror=MSE)$groups





### Off-diagonal element analysis regarding SAD

STFb<-matrix(NA, 13 ,6) ### Total Facial Emotions for Batch (7 emotion in # of frames)
STFc<-matrix(NA, 13,6)	### Total Facial Emotions for Continuous (7 emotion in # of frames)



Sb<-c("T016","T021","T051","T077","T079","T083","T085","T106","T139","T144","T166","T175","T178")
Sc<-c("T005","T063","T064","T068","T092","T094","T098","T099","T121","T124","T132","T151","T157")

setwd(file.path(current_dir, 'curated-data', 'Subj Data', 'Batch', 'DT'))
tmpc<-1
for (i in Sb){
	tmpB1<-read.csv(paste(i,".csv",sep=""),header=T,sep=",")
	STFb[tmpc,1:6]<-c(tmpB1[1,6],tmpB1[2,6],tmpB1[3,6],tmpB1[4,6],tmpB1[5,7],tmpB1[5,8])
	tmpc<-tmpc+1
}

setwd(file.path(current_dir, 'curated-data', 'Subj Data', 'Continual', 'DT'))
tmpc<-1
for (i in Sc){
	tmpB1<-read.csv(paste(i,".csv",sep=""),header=T,sep=",")
	STFc[tmpc,1:6]<-c(tmpB1[1,6],tmpB1[2,6],tmpB1[3,6],tmpB1[4,6],tmpB1[5,7],tmpB1[5,8])
	tmpc<-tmpc+1
}


STB<-cbind(c(STFb[,1:6]),rep(1:6,each=13),rep(0,6*13))
STC<-cbind(c(STFc[,1:6]),rep(1:6,each=13),rep(1,6*13))

STF<-rbind(STB,STC)
STFper<-rbind(colSums(STFb[,1:6]), colSums(STFc[,1:6]))

# quartz()
# windows()
new_display(os)
interaction.plot(STF[,3],STF[,2],STF[,1], type="b", col=c(7:1), leg.bty="o", lwd=2, pch=c(18,24,22,17,15,16,19),xlab="Group", ylab="Mean of Frames", main="Interaction plot of mean X per levels of factors A and B")


fit0<-aov((STF[,1]+1)~ factor(STF[,2])* factor(STF[,3]))
fit0
summary(fit0)	### anova table

### BOX-COX
# quartz()
# windows()
new_display(os)
boxcox(fit0)

fit<-aov(log(STF[,1]+1)~ factor(STF[,2])* factor(STF[,3]))
fit
summary(fit)	### anova table

### Test the normality assumption in the residuals
# quartz()
# windows()
new_display(os)
qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)
shapiro.test(fit$residuals)


### Test the assumption of homogeneity of variances
bartlett.test(log(STF[,1]+1)~factor(STF[,2]))
bartlett.test(log(STF[,1]+1)~factor(STF[,3]))
fligner.test(log(STF[,1]+1)~factor(STF[,2]))
fligner.test(log(STF[,1]+1)~factor(STF[,3]))


### Diagnostic Plots
# quartz()
# windows()
new_display(os)
layout(matrix(1:4,2,2))
plot(fit)

