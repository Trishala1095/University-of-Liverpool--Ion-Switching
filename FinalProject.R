library(dplyr)
library(DataExplorer)
library(ggplot2)
library(data.table)
library(caret)
library(e1071)
library(klaR)

train<-read.csv("train.csv")
test<-read.csv("test.csv")
#checking for NA values
sum(is.na(train))
sum(is.na(test))
introduce(train)
#trying to visualize smapled train data
train %>% sample_n(5000)%>%
  ggplot(aes(x = time, y = signal))+
  geom_line(color = "blue")+
  geom_vline(xintercept = seq(0, 500, 50))
#Dividing train to batches
#t<-seq(50,500,by=50)
#batch<-lapply(seq_along(t),function(i)train[(t-49)[i]:t[i],])

## plot for signal and number of open channels for all batches
ggplot(train,aes(x = time,y = signal,col="Signal"))+
  geom_point(shape=".")+
  geom_point(aes(y=open_channels,col="Open channels"),shape=".")+
  scale_y_continuous(sec.axis=sec_axis(~.,name="open channels"))+
  labs(title="Signal and number of open channels per time for all batches", x="time", y="signal")
#here we can see that as the signal value increase so do the no. of open channels for batches 0-5 but thats not the case for 
#batches 6-9, probably coz of the signal pattern

## time Vs open channel 
ggplot(train,aes(x = time, y = open_channels)) + 
  geom_line(colour="blue") +
  labs(title="training data") 

##adding feature batches
train$batches<- as.factor((train$time - 0.0001)%/%50)
ggplot(train,aes(x=batches,y=signal))+
  geom_boxplot()+
  labs(title="Signal value distribution per batches", x="batches", y="signal")

##time vs signal plot for test data
ggplot(test,aes(x = time,y = signal,col="Signal"))+
  geom_point(shape=".")+
  labs(title="Signal per time for all batches for the Test data", x="time", y="signal")
#so we see here that the trend the test data follows is different than train data so we can't assign batches here like train data

#plotting batch 1
ggplot(subset(train,train$batches==1),aes(x = time,y = signal,col="Signal"))+
  geom_point(shape=".")+
  geom_point(aes(y=open_channels,col="Open channels"),shape=".")+
  scale_y_continuous(sec.axis=sec_axis(~.,name="open channels"))+
  labs(x="time",y="signal")

##detrending batch 1
#train1<-subset(train,train$batches==1)
trend.lm<-lm(signal~poly(time,2),data=subset(train,train$batches==1 & train$time>=50 & train$time<60))
train$res[train$batches==1 & train$time>=50 & train$time<60]<-trend.lm$residuals
train$fit[train$batches==1 & train$time>=50 & train$time<60]<-trend.lm$fitted.values

trend.lm<-lm(signal~time,data=subset(train,train$batches==1 & train$time>=60))
train$res[train$batches==1 & train$time>=60]<-trend.lm$residuals
train$fit[train$batches==1 & train$time>=60]<-trend.lm$fitted.values

## for batches 0 and 2 to 5
#train2<-subset(train,train$batches==c(2:5))
for(i in c(0,2:5)){
  trend.lm<-lm(signal~time,data=subset(train,batches==i))
  train$res[train$batches==i] <- trend.lm$residuals
  train$fit[train$batches==i]<-trend.lm$fitted.values
}

## for batches 6 to 9
#train3<-subset(train,train$batches==c(6:9))
for(i in c(6:9)){
  trend.lm<-lm(signal~poly(time,2),data=subset(train,batches==i))
  train$res[train$batches==i] <- trend.lm$residuals
  train$fit[train$batches==i]<-trend.lm$fitted.values
}

ggplot(train,aes(x = time,y = res,col="detrended signal"))+
  geom_point(shape=".")+
  labs(title ="Detrended signal",x="time",y="residuals")

ggplot(train,aes(x=batches,y=res))+
  geom_boxplot()+
  labs(main="distribution per batches", x="batches", y="signal residuals")

set.seed(123)
train.inx<-createDataPartition(train$signal,p = 0.8,list = F,times = 1)
#smp_size<-floor(0.80*nrow(train))
#train.inx<-sample(seq_len(smp_size),size = smp_size)
train1<-train[train.inx,]
train$open_channels<-as.factor(train$open_channels)
test1<-train[-train.inx,]
#actual<-test1[,3]
#test1<-select(test1,-open_channels)


# cv5<- trainControl(method="cv",number=5)
# model1<-train(open_channels~res+batches,data=train1,method="lda",family="binomial", trControl=cv5)
# #model1$results["Accuracy"]
# valid<-predict(model1,newdata = test1)
# p<-data.frame(actual,valid)
# acc=mean(p$actual==p$valid)

#rf1 <- randomForest(open_channels~res+batches,data = train1,ntree=55,importance=TRUE)

#Naive bayes
# x=train1[,-3]
# y=train1$open_channels
# train1$open_channels<-as.numeric(train1$open_channels)
# model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#ANOVA
model<- aov(open_channels~res+batches,data = train1)
summary(model)
Predict <- predict(model,newdata = test1 )
confusionMatrix(Predict, test1$open_channels)




