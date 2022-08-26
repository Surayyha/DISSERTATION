#data
data<-read.table("C:/Users/as/Documents/s2/DISSERTATION/participants.csv",header = T,
                 sep = ",",quote = "")
participant<-unique(data$Participant)
data$Group<-as.factor(data$Group)
data$Participant<-as.factor(data$Participant)
data$Location<-as.factor(data$Location)
data$Task<-as.factor(data$Task)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(moments)
library(ggtern)
library("compositions")

data<-data[,-c(2,5,7)]
data$Group<-factor(data$Group, levels = c("Alzheimer's", "MCI", "Control"))
data$Task<-factor(data$Task, levels = c("Clothes", "Heads","Free"))
## summary
summary(data)

## area plot
#### proportion of each frame 
propor<-data%>%
  group_by(Frame, Task, Group)%>%
  summarise(Clothes=mean(Location=="Clothes"),Bald_Heads=mean(Location=="Bald Heads"),
            Other=mean(Location=="Other"),No_Trace=mean(Location=="No Trace"))%>%
  gather(key="Location",value="por",Clothes:No_Trace)

propor$Location<-factor(propor$Location, levels = c("Clothes", "Bald_Heads","Other","No_Trace"))
## draw areaplots
ggplot(propor, aes(x=Frame, y=por, fill=Location)) + 
  geom_area(alpha=0.6 , size=1)+
  facet_grid(Task~Group)


## ternary
length(unique(data$Frame))
space=seq(1, 1199, length.out =  11)

n=0
for (i in space[2:11]) {
  n=n+1
  train_first<-data%>%
    filter(Frame<=i)%>%
    group_by(Participant,Group,Task)%>%
    summarise(Clothes=mean(Location=="Clothes"),Bald_Heads=mean(Location=="Bald Heads"),
              Out_Region=mean(Location=="Other"|Location=="No Trace"))
  
  mean_frame<-train_first%>%
    group_by(Task,Group)%>%
    summarise(Clothes=mean(Clothes),Bald_Heads=mean(Bald_Heads),Out_Region=mean(Out_Region))
    
  p<-ggtern(train_first, aes(Clothes,Bald_Heads,Out_Region,color=Group)) + 
      theme_rgbw() + 
      geom_point(size=0.6,alpha=0.5) +
      geom_point(data=mean_frame, aes(Clothes,Bald_Heads,Out_Region),size=1.2)+
      geom_mean_ellipse()+
      labs(subtitle = paste("time stamp:", n, sep=" "))+
      facet_grid(~Task)
  print(p)
}


task=levels(data$Task)
group=levels(data$Group)

##
ter_mean<-c()
n=0
for (i in space[2:11]) {
  n=n+1
  pro<-data%>%
    filter(Frame<=i)%>%
    group_by(Participant,Group,Task)%>%
    summarise(clothes=mean(Location=="Clothes"),heads=mean(Location=="Bald Heads"),
              outregion=mean(Location=="Other"|Location=="No Trace"))
  pro_mean<-pro%>%
    group_by(Group,Task)%>%
    summarise(Clothes=mean(clothes),Bald_Heads=mean(heads),
              Out_Region=mean(outregion))
  pro_mean[,"Frame"]<-rep(n,nrow(pro_mean))
  ter_mean<-rbind(ter_mean,pro_mean)
}
ter_mean=ter_mean[order(ter_mean$Group,ter_mean$Task),]
##pathplot
ggtern(data=ter_mean,aes(Clothes,Bald_Heads,Out_Region,color=Group)) + 
        geom_path()+
        geom_point(size=0.2) +
        geom_text(data=ter_mean[ter_mean$Frame==1,],aes(Clothes,Bald_Heads,Out_Region,label="1"),
                  color="black",size=3)+
        labs(title="Changes of means along time")+
        theme_classic()+
        facet_grid(~Task) 



#### correspondence analysis
library("FactoMineR")
library("factoextra")
par(mfrow=c(2,2))

for (i in levels(data$Task)){
  sub_d<-data%>%
    filter(Task==i)
  ca_cro<-xtabs(~Group+Location,sub_d[,3:4])
  ca_res<-CA(ca_cro, graph = FALSE)
  p<-fviz_ca_biplot(ca_res, repel = TRUE)+
    labs(title=paste("Task:",i))
  show(p)
  #title(cbind("Task:",i))
}

for (i in levels(data$Task)){
  sub_d<-data%>%
    filter(Task==i)
  ca_cro<-xtabs(~Group+Location,sub_d[,3:4])
  ca_res<-CA(ca_cro, graph = FALSE)
  p<-fviz_ca_biplot(ca_res, 
                    map ="colprincipal", arrow = c(TRUE, TRUE),
                    repel = TRUE)+
    labs(title=paste("Task:",i))
  show(p)
  #title(cbind("Task:",i))
}

###features
gaze<-data%>%
  group_by(Participant,Task)%>%
  summarise(Group=unique(Group),head_duration=0, clothes_duration=0,notrace_duration=0,
            other_duration=0,head_num=0,clothes_num=0,notrace_num=0,other_num=0)

gazing <- function(loc) {
  num=0
  w<-c(which(loc==1),2000)
  if (sum(loc)==0){
    duration=0
  }else{
    gaze_dur<-c()
    m=length(w)
    ori<-w[1]
    end<-w[1]
    #if(m>1){
      for (i in 2:m){
        if (w[i]!=(w[i-1]+1)){
          end<-w[i-1]
          dur<-end-ori+1
          ori<-w[i]
          gaze_dur<-c(gaze_dur, dur)
          num=num+1
        }
      }
      duration=mean(gaze_dur)
    #}
    # else{
    #   num=0
    #   duration=1
    #   stand=0
    # }
    # if(num==0){
    #   duration=0
    #   stand=NA
    # }
  }
  return(c(duration,num))
}

task<-levels(data$Task)
location<-levels(data$Location)
n=nrow(gaze)

for (i in 1:n) {
  d<-data%>%
    filter(Participant==gaze$Participant[i]&Task==gaze$Task[i])%>%
    dplyr::select(Location)
  for (l in 1:4) {
    loc<-d==location[l]
    result<-gazing(loc)
    gaze[i,3+l]<-result[1]
    gaze[i,7+l]<-result[2]
  }
}


#####
## correlation
library(psych)
gaze%>%
  group_by(Group)%>%
  
corPlot(gaze[,4:19], cex = 1)

lapply(split(gaze[,4:19],gaze$Group),cor)

## anova
### free
free<-gaze%>%
  filter(Task=="Free")
m<-aov(clothes_duration ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(head_duration ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(other_duration ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(notrace_duration ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(clothes_num ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(head_num ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(other_num ~ Group,data=free)
summary(m)
TukeyHSD(m)

m<-aov(notrace_num ~ Group,data=free)
summary(m)
TukeyHSD(m)


### clothes
clothes<-gaze%>%
  filter(Task=="Clothes")
m<-aov(clothes_duration ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(head_duration ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(other_duration ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(notrace_duration ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(clothes_num ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(head_num ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(other_num ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

m<-aov(notrace_num ~ Group,data=clothes)
summary(m)
TukeyHSD(m)

### heads
heads<-gaze%>%
  filter(Task=="Heads")
m<-aov(clothes_duration ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(head_duration ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(other_duration ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(notrace_duration ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(clothes_num ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(head_num ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(other_num ~ Group,data=heads)
summary(m)
TukeyHSD(m)

m<-aov(notrace_num ~ Group,data=heads)
summary(m)
TukeyHSD(m)


## fitted model
library(MASS)
library(e1071)
library(caret)
library(boot)

####clothes
tab_ld_clothes=0
tab_qd_clothes=0
tab_svm_clothes=0
acc_clothes<-data.frame(SVM=rep(0,100),LD=rep(0,100),QD=rep(0,100))
acc_group_clothes<- data.frame(Classifier=rep(c("SVM","LD","QD"),100),Alzheimer=rep(0,300),
                               MCI=rep(0,300),control=rep(0,300))


task_data<-gaze%>%
  filter(Task=="Clothes")
set.seed(100)
trainIndex <- createDataPartition(task_data$Group, p = .8,
                                  list = FALSE,
                                  times = 100)
for (i in 1:100){
  training<-task_data[trainIndex[,i],]
  testing<-task_data[-trainIndex[,i],]
  
  ##SVM
  set.seed(1)
  tune_out = tune(svm, 
                  Group~., 
                  data = training[,3:11], 
                  kernel = "linear", 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
  bestmod = tune_out$best.model
  summary(bestmod)
  class_pred = predict(bestmod, testing[,3:11])
  tab_svm=table(predicted = class_pred, true = testing$Group)
  acc_clothes[i,"SVM"]<-sum(diag(tab_svm))/sum(tab_svm)
  acc_group_clothes[i*3-2,2:4]<-diag(prop.table(tab_svm, 2))
  tab_svm_clothes=tab_svm_clothes+tab_svm
  
  ##LD
  ld<-lda(Group~., training[,3:11])
  pre_ld <- predict(ld, testing[,3:11])$class
  tab_ld <- table(Predicted = pre_ld, Actual = testing$Group)
  acc_clothes[i,"LD"]<-sum(diag(tab_ld))/sum(tab_ld)
  acc_group_clothes[i*3-1,2:4]<-diag(prop.table(tab_ld, 2))
  tab_ld_clothes=tab_ld_clothes+tab_ld
  
  ##QD
  qd<-qda(Group~., data=training[,3:11])
  pre_qd <- predict(qd, testing[,3:11])$class
  tab_qd <- table(Predicted = pre_qd, Actual = testing$Group)
  acc_clothes[i,"QD"]<-sum(diag(tab_ld))/sum(tab_ld)
  acc_group_clothes[i*3,2:4]<-diag(prop.table(tab_qd, 2))
  tab_qd_clothes=tab_qd_clothes+tab_qd
}

###heads
tab_ld_heads=0
tab_qd_heads=0
tab_svm_heads=0
acc_heads<-data.frame(SVM=rep(0,100),LD=rep(0,100),QD=rep(0,100))
acc_group_heads<- data.frame(Classifier=rep(c("SVM","LD","QD"),100),Alzheimer=rep(0,300),
                               MCI=rep(0,300),control=rep(0,300))

task_data<-gaze%>%
  filter(Task=="Heads")
set.seed(100)
trainIndex <- createDataPartition(task_data$Group, p = .8,
                                  list = FALSE,
                                  times = 100)
for (i in 1:100){
  training<-task_data[trainIndex[,i],]
  testing<-task_data[-trainIndex[,i],]
  
  ##SVM
  set.seed(1)
  tune_out = tune(svm, 
                  Group~., 
                  data = training[,3:11], 
                  kernel = "linear", 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
  bestmod = tune_out$best.model
  summary(bestmod)
  class_pred = predict(bestmod, testing[,3:11])
  tab_svm=table(predicted = class_pred, true = testing$Group)
  acc_heads[i,"SVM"]<-sum(diag(tab_svm))/sum(tab_svm)
  acc_group_heads[i*3-2,2:4]<-diag(prop.table(tab_svm, 2))
  tab_svm_heads=tab_svm_heads+tab_svm
  
  ##LD
  ld<-lda(Group~., training[,3:11])
  pre_ld <- predict(ld, testing[,3:11])$class
  tab_ld <- table(Predicted = pre_ld, Actual = testing$Group)
  acc_heads[i,"LD"]<-sum(diag(tab_ld))/sum(tab_ld)
  acc_group_heads[i*3-1,2:4]<-diag(prop.table(tab_ld, 2))
  tab_ld_heads=tab_ld_heads+tab_ld
  
  ##QD
  qd<-qda(Group~., data=training[,3:11])
  pre_qd <- predict(qd, testing[,3:11])$class
  tab_qd <- table(Predicted = pre_qd, Actual = testing$Group)
  acc_heads[i,"QD"]<-sum(diag(tab_ld))/sum(tab_ld)
  acc_group_heads[i*3,2:4]<-diag(prop.table(tab_qd, 2))
  tab_qd_heads=tab_qd_heads+tab_qd
}

##free
tab_ld_free=0
tab_qd_free=0
tab_svm_free=0
acc_free<-data.frame(SVM=rep(0,100),LD=rep(0,100),QD=rep(0,100))
acc_group_free<- data.frame(Classifier=rep(c("SVM","LD","QD"),100),Alzheimer=rep(0,300),
                             MCI=rep(0,300),control=rep(0,300))

task_data<-gaze%>%
  filter(Task=="Free")
set.seed(100)
trainIndex <- createDataPartition(task_data$Group, p = .8,
                                  list = FALSE,
                                  times = 100)
for (i in 1:100){
  training<-task_data[trainIndex[,i],]
  testing<-task_data[-trainIndex[,i],]
  
  ##SVM
  set.seed(1)
  tune_out = tune(svm, 
                  Group~., 
                  data = training[,3:11], 
                  kernel = "linear", 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
  bestmod = tune_out$best.model
  summary(bestmod)
  class_pred = predict(bestmod, testing[,3:11])
  tab_svm=table(predicted = class_pred, true = testing$Group)
  acc_free[i,"SVM"]<-sum(diag(tab_svm))/sum(tab_svm)
  acc_group_free[i*3-2,2:4]<-diag(prop.table(tab_svm, 2))
  tab_svm_free=tab_svm_free+tab_svm
  
  ##LD
  ld<-lda(Group~., training[,3:11])
  pre_ld <- predict(ld, testing[,3:11])$class
  tab_ld <- table(Predicted = pre_ld, Actual = testing$Group)
  acc_free[i,"LD"]<-sum(diag(tab_ld))/sum(tab_ld)
  acc_group_free[i*3-1,2:4]<-diag(prop.table(tab_ld, 2))
  tab_ld_free=tab_ld_free+tab_ld
  
  ##QD
  qd<-qda(Group~., data=training[,3:11])
  pre_qd <- predict(qd, testing[,3:11])$class
  tab_qd <- table(Predicted = pre_qd, Actual = testing$Group)
  acc_free[i,"QD"]<-sum(diag(tab_ld))/sum(tab_ld)
  acc_group_free[i*3,2:4]<-diag(prop.table(tab_qd, 2))
  tab_qd_free=tab_qd_free+tab_qd
}

### overall accuracy
par(mfrow=c(2,2))

acc<-rbind(acc_clothes,acc_heads,acc_free)
acc[,"task"]<-rep(c("Clothes","Heads","Free"),each=100)
acc[,"task"]<-factor(acc$task, levels = c("Clothes", "Heads", "Free"))
acc%>%
  gather(key="Classifier",value ="accuracy",SVM:QD)%>%
  ggplot(aes(x=Classifier, y=accuracy, fill=task))+
  geom_boxplot()+
  labs(y="Accuracy")+
  geom_hline(yintercept=0.33,color="red")

##group precision
acc_group<-rbind(acc_group_clothes,acc_group_heads,acc_group_free)
acc_group[,"task"]<-rep(c("Clothes","Heads","Free"),each=300)
acc_group[,"task"]<-factor(acc_group$task, levels = c("Clothes", "Heads", "Free"))
colnames(acc_group)[4]<-"Control"
acc_group<-acc_group%>%
  gather(key="group",value ="accuracy",Alzheimer:Control)
acc_group$group<-factor(acc_group$group, levels = c("Alzheimer", "MCI", "Control"))
ggplot(data=acc_group,aes(x=Classifier, y=accuracy,fill=group))+
  geom_boxplot()+
  geom_hline(yintercept=0.33,color="red")+
  labs(y="Precision")+
  facet_wrap(~task)

##mean accuracy
mean(acc_clothes$SVM)
mean(acc_clothes$LD)
mean(acc_clothes$QD)

mean(acc_heads$SVM)
mean(acc_heads$LD)
mean(acc_heads$QD)

mean(acc_free$SVM)
mean(acc_free$LD)
mean(acc_free$QD)

## mean group accuracy
acc_group_clothes%>%
  group_by(Classifier)%>%
  summarise(Alzheimer=mean(Alzheimer),MCI=mean(MCI),Control=mean(control))

acc_group_heads%>%
  group_by(Classifier)%>%
  summarise(Alzheimer=mean(Alzheimer),MCI=mean(MCI),Control=mean(control))

acc_group_free%>%
  group_by(Classifier)%>%
  summarise(Alzheimer=mean(Alzheimer),MCI=mean(MCI),Control=mean(control))

