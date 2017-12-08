setwd("/Users/nugthug/Documents/cmpsci/348/Assignment/06/")

library(bnlearn)
library(plyr)
library(Rgraphviz)
housing <- read.csv("housing-2017.data", header=F, sep=" ")

colnames(housing) <- c("crime", "zoned", "industry", "river", "nox", "rooms", "age", "dist", "highways", "tax", "ptratio", "lstatus", "medval")
housing$river <- as.factor(housing$river)
housing$highways <- as.numeric(housing$highways)
housing$tax<-as.numeric(housing$tax)
housing.temp <- discretize(housing[,!(colnames(housing) %in% c("zoned", "highways"))], method = "quantile", breaks=4)
housing <- cbind(housing.temp, discretize(housing[,colnames(housing) %in% c("zoned", "highways")], method = "interval", breaks=4))

#i wrote all this code without stack overflow.
#question 1
hc1<-hc(housing)
hc1.fit<-bn.fit(hc1,housing)


graphviz.plot(hc1)

levels(housing$industry)
table(housing$industry)
levels(housing$nox)


q2.1<-cpdist(hc1.fit,nodes = "nox",evidence=(industry=="[0.46,5.19]"))
q2.2<-cpdist(hc1.fit,nodes="nox",evidence=(industry=="(5.19,9.69]"))
q2.3<-cpdist(hc1.fit,nodes="nox",evidence=(industry=="(9.69,18.1]"))


plot(q2.1,main="nox Distribution at [0.46,5.19] industry")
plot(q2.2,main="nox Distribution at (5.19,9.69] industry")
plot(q2.3,main="nox Distribution at (9.69,18.1] industry")

cpquery(hc1.fit,event=(nox=="[0.385,0.449]"),evidence=(industry=="[0.46,5.19]"))
cpquery(hc1.fit,event=(nox=="[0.385,0.449]"),evidence=(industry=="(5.19,9.69]"))
cpquery(hc1.fit,event=(nox=="[0.385,0.449]"),evidence=(industry=="(9.69,18.1]"))

cpquery(hc1.fit,event=(nox== "(0.449,0.538]"),evidence=(industry=="[0.46,5.19]"))
cpquery(hc1.fit,event=(nox== "(0.449,0.538]"),evidence=(industry=="(5.19,9.69]"))
cpquery(hc1.fit,event=(nox== "(0.449,0.538]"),evidence=(industry=="(9.69,18.1]"))

cpquery(hc1.fit,event=(nox== "(0.538,0.624]"),evidence=(industry=="[0.46,5.19]"))
cpquery(hc1.fit,event=(nox== "(0.538,0.624]"),evidence=(industry=="(5.19,9.69]"))
cpquery(hc1.fit,event=(nox== "(0.538,0.624]"),evidence=(industry=="(9.69,18.1]"))


cpquery(hc1.fit,event=(nox== "(0.624,0.871]"),evidence=(industry=="[0.46,5.19]"))
cpquery(hc1.fit,event=(nox== "(0.624,0.871]"),evidence=(industry=="(5.19,9.69]"))
cpquery(hc1.fit,event=(nox== "(0.624,0.871]"),evidence=(industry=="(9.69,18.1]"))


q3Inter<-drop.arc(hc1,from="crime",to="industry")
q3.fit<-bn.fit(q3Inter,housing)
graphviz.plot(q3.fit)


cpquery(q3.fit,event=(nox=="[0.385,0.449]"),evidence=(industry=="[0.46,5.19]"))
cpquery(q3.fit,event=(nox=="[0.385,0.449]"),evidence=(industry=="(5.19,9.69]"))
cpquery(q3.fit,event=(nox=="[0.385,0.449]"),evidence=(industry=="(9.69,18.1]"))

cpquery(q3.fit,event=(nox== "(0.449,0.538]"),evidence=(industry=="[0.46,5.19]"))
cpquery(q3.fit,event=(nox== "(0.449,0.538]"),evidence=(industry=="(5.19,9.69]"))
cpquery(q3.fit,event=(nox== "(0.449,0.538]"),evidence=(industry=="(9.69,18.1]"))

cpquery(q3.fit,event=(nox== "(0.538,0.624]"),evidence=(industry=="[0.46,5.19]"))
cpquery(q3.fit,event=(nox== "(0.538,0.624]"),evidence=(industry=="(5.19,9.69]"))
cpquery(q3.fit,event=(nox== "(0.538,0.624]"),evidence=(industry=="(9.69,18.1]"))


cpquery(q3.fit,event=(nox== "(0.624,0.871]"),evidence=(industry=="[0.46,5.19]"))
cpquery(q3.fit,event=(nox== "(0.624,0.871]"),evidence=(industry=="(5.19,9.69]"))
cpquery(q3.fit,event=(nox== "(0.624,0.871]"),evidence=(industry=="(9.69,18.1]"))


q3.1<-cpdist(q3.fit,nodes = "nox",evidence=(industry=="[0.46,5.19]"))
q3.2<-cpdist(q3.fit,nodes="nox",evidence=(industry=="(5.19,9.69]"))
q3.3<-cpdist(q3.fit,nodes="nox",evidence=(industry=="(9.69,18.1]"))

plot(q3.1,main="nox Distribution at [0.46,5.19] industry intervention")
plot(q3.2,main="nox Distribution at (5.19,9.69] industry intervention")
plot(q3.3,main="nox Distribution at (9.69,18.1] industry intervention")



plot(q3.1)
plot(q2.1)

plot(q2.2)
plot(q3.2)

plot(q2.3)
plot(q3.3)


q4InterA<-drop.arc(hc1,from="crime",to="nox")
q4InterB<-drop.arc(q4InterA,from="industry", to="nox")
q4.fit<-bn.fit(q4InterB,housing)
graphviz.plot(q4.fit)

q4a.1<-cpdist(hc1.fit,nodes = "industry",evidence=(nox=="[0.385,0.449]"))
q4a.2<-cpdist(hc1.fit,nodes="industry",evidence=(nox=="(0.449,0.538]"))
q4a.3<-cpdist(hc1.fit,nodes="industry",evidence=(nox=="(0.538,0.624]"))
q4a.4<-cpdist(hc1.fit,nodes="industry",evidence=(nox=="(0.624,0.871]"))
levels(housing$nox)


q4b.1<-cpdist(q4.fit,nodes = "industry",evidence=(nox=="[0.385,0.449]"))
q4b.2<-cpdist(q4.fit,nodes="industry",evidence=(nox=="(0.449,0.538]"))
q4b.3<-cpdist(q4.fit,nodes="industry",evidence=(nox=="(0.538,0.624]"))
q4b.4<-cpdist(q4.fit,nodes="industry",evidence=(nox=="(0.624,0.871]"))

plot(q4a.1,main="industry Distribution with nox at [0.385,0.449]")
plot(q4b.1,main="industry Distribution with nox at [0.385,0.449] intervention")


plot(q4a.2,main="industry Distribution with nox at (0.449,0.538]")
plot(q4b.2,main="industry Distribution with nox at (0.449,0.538] intervention")

plot(q4a.3, main="industry Distribution with nox at (0.538,0.624]")
plot(q4b.3,main="industry Distribution with nox at (0.538,0.624] intervention")

plot(q4a.4,main="industry Distribution with nox at (0.624,0.871]")
plot(q4b.4,main="industry Distribution with nox at (0.624,0.871] intervention")

q5<-iamb(housing)
q5
graphviz.plot(q5)
