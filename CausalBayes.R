setwd("/Users/nugthug/Documents/cmpsci/348/Assignment/07")


library(plyr)
library(rpart)
library(ggplot2)

drop(housing)
housing <- read.csv("housing-2017.data", header=F, sep=" ")
colnames(housing) <- c("crime", "zoned", "industry", "river", "nox", "rooms", "age", 
                       "dist", "highways", "tax", "ptratio", "lstatus", "medval")
housing$river <- as.factor(housing$river)
housing$highways <- as.numeric(housing$highways)
housing$is.zoned <- housing$zoned != 0
housing$zoned <- NULL
#1 code below
ggplot(housing, aes(x=crime))+
  geom_histogram(binwidth = 5)+
  ggtitle("Crime by is zoned")+
  xlab("Crime")+
  ylab("Count")+
  facet_grid(.~is.zoned)
  theme( legend.title = element_blank())
  
#appears to be a dependency because if you know that crime
  # is above zero, you know that is.zoned is definately false
summary(housing)  

housing$is.zoned<-as.integer(housing$is.zoned)
q2.fit<-  wilcox.test(crime~is.zoned, data=housing)
q2.fit
library(caret)
library(cvTools)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(rattle)
#library(e1071)

fitcontrol<-trainControl(
  method = 'repeatedcv',
  number = 3,
  repeats= 10,
  savePredictions = FALSE)

#note change outcome back to factor
#housing$is.zoned<-factor(housing$is.zoned, labels=c("yes","no"))
housing$river<-as.numeric(housing$river)

q3.fit<-train(is.zoned~industry+river+nox+rooms+age+dist+highways+tax+ptratio+lstatus+medval,
              data=housing,
              method="rpart",
              trControl=fitcontrol)

#q3.fit$finalModel$
#summary(q3.fit$finalModel)
plot(q3.fit)


#q3.fit$finalModel$xNames<-c("crime", "industry", "river", "nox", "rooms", "age", 
 #                           "dist", "highways", "tax", "ptratio", "lstatus", "medval","is.zoned")
q3.fit$finalModel$terms
q3.best.fit<-predict(q3.fit$finalModel,housing,type='vector')

q3.best.fit
prop.scores<-data.frame(q3.best.fit)

ggplot(prop.scores, aes(x=q3.best.fit))+
  geom_histogram(binwidth = .05)+
  ggtitle("Dist of scores")+
  xlab("Propensity Scores")+
  ylab("Count")+
  theme( legend.title = element_blank())


rpart.plot(q3.fit$finalModel)
rpartGrid2<-expand.grid(
  maxdepth=c(6)
)

q4.depth.fit<-train(is.zoned~. -crime ,
                    data=housing,
                    method="rpart2",
                    trControl=fitcontrol,
                    tuneGrid=rpartGrid2
)

q4.depth.fit.predict<-predict(q4.depth.fit$finalModel,housing,type='vector')
prop.scores<-data.frame(q4.depth.fit.predict)



ggplot(prop.scores, aes(x=q4.depth.fit.predict))+
  geom_histogram(binwidth = .01)+
  ggtitle("Dist of scores")+
  xlab("Propensity Scores")+
  ylab("Count")+
theme( legend.title = element_blank())


# q3.rpart.test<-rpart(is.zoned~.-crime,housing,xval=10)
# q3.rpart.pred<-predict(q3.rpart.test,housing,type='vector')
# prop.score<-xpred.rpart(q3.rpart.test, xval = 10, return.all = FALSE)
# q3.rpart.df<-data.frame(q3.rpart.pred)
# ggplot(q3.rpart.df, aes(x=q3.rpart.pred))+
#   geom_histogram(binwidth = .01)+
#   ggtitle('Dist of scores rpart')+
#   xlab('Propensity Scores')+
#   ylab('Count')+
#   theme( legend.title = element_blank())

# table(q3.rpart.df$q3.rpart.pred,housing$is.zoned)

table(prop.scores$q4.depth.fit.predict,housing$is.zoned)

#below is from http://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function-in-r-program

set.seed(123)

smp_size <- floor(0.75 * nrow(housing))


train_ind <- sample(seq_len(nrow(housing)), size = smp_size)

train <- housing[train_ind, ]
test <- housing[-train_ind, ]

q3.rpart.train<-rpart(is.zoned~.-crime,train)
q3.rpart.test<-predict(q3.rpart.train,test,type='vector')
q3.rpart.df<-data.frame(q3.rpart.test)
ggplot(q3.rpart.df, aes(x=q3.rpart.test))+
  geom_histogram(binwidth = .01)+
  ggtitle('Dist of scores rpart')+
  xlab('Propensity Scores')+
  ylab('Count')+
  theme( legend.title = element_blank())

table(q3.rpart.df$q3.rpart.pred,housing$is.zoned)

q3.all.prop<-predict(q3.rpart.train,housing,type='vector')


q3.rpart.df<-data.frame(q3.all.prop)
ggplot(q3.rpart.df, aes(x=q3.all.prop))+
  geom_histogram(binwidth = .01)+
  ggtitle('Dist of scores rpart')+
  xlab('Propensity Scores')+
  ylab('Count')+
  theme( legend.title = element_blank())


table(q3.rpart.df$q3.rpart.pred,housing$is.zoned)


q5<-cut(prop.scores$q4.depth.fit.predict,3)
df.q5<-data.frame(q5)


ggplot(df.q5, aes(x=q5))+
  geom_histogram(stat='count')+
  ggtitle('Dist of scores rpart')+
  xlab('Propensity Scores')+
  ylab('Count')+
  theme( legend.title = element_blank())



q5<-cut(prop.scores$q4.depth.fit.predict,3)
housing$propScore<-q5

levels(housing$propScore)
q5.q1<-subset(housing, housing$propScore=="(-0.001,0.333]")
q5.q2<-subset(housing, housing$propScore=="(0.333,0.667]")
q5.q3<-subset(housing, housing$propScore=="(0.667,1]")


q5.fit.1<-  wilcox.test(crime~is.zoned, data=q5.q1)
q5.fit.2<-  wilcox.test(crime~is.zoned, data=q5.q2)
q5.fit.3<-  wilcox.test(crime~is.zoned, data=q5.q3)


q5.fit.1
q5.fit.2
q5.fit.3

ggplot(q5.q2, aes(x=crime, fill=as.factor(is.zoned)))+
  geom_histogram(stat='count')+
  ggtitle('Crime rate by is.zoned in middle propensity score')+
  xlab('Crime')+
  ylab('Count')+
  #facet_grid(.~as.factor(is.zoned))
  theme( legend.title = element_blank())
