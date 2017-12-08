setwd("/Users/nugthug/Documents/cmpsci/348/Assignment/03")
library(fpc)
library(tidyverse)
df <- read.csv("isolet1+2+3+4.data")
colnames(df) <- c(paste("val_",1:(ncol(df)-1), sep=""), "letter")
df
df2<-select(df,-letter)

k.max <- 30 # Maximal number of clusters
#http://www.sthda.com/english/wiki/print.php?id=239 
wss <- sapply(1:k.max, function(k){kmeans(df2, k, nstart=25 )$tot.withinss})
saveRDS(wss,'kmeans1to30.rds')
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

km.at.2<-kmeans(df2,2,nstart=25)
saveRDS(km.at.2,'kmAt2.rds')
km.at.2

km.at.3<-kmeans(df2,3,nstart=25)
saveRDS(km.at.3,'kmat3.rds')

#Attempting a pamk run to find best clustering number
pamkRun<-pamk(df2,krange=1:30,criterion = 'asw',usepam=FALSE)
saveRDS(pamkRun,"pamkRun.rds")


pamkRun

pamkRunMultAsw<-pamk(df2,krange=1:30,criterion = 'multiasw',usepam=FALSE)
saveRDS(pamkRun,"pamkRunMultAsw.rds")

pamkRunMultAsw



harch<-hclust(dist(df2),method='complete')
saveRDS(harch,'harchComplete.rds')

harch
plot(harch)


hcut<-cutree(harch,8)
table(km.at.2$cluster,hcut)

km.at.26<-kmeans(df2,26,nstart=25)

km.at.26


hcut26<-cutree(harch,26)
table(km.at.26$cluster,hcut26)


#install.packages('EMCluster')
#install.packages('mclust')

#initial run of Mclust
library(mclust)
emclust<-Mclust(df2)
saveRDS(emclust,'emclust.RDS')
emclust




emclust<-loadRDS('emclust.RDS')

hcut2<-cutree(harch,2)


table(km.at.2$cluster,hcut2)

table(pamkRunMultAsw$pamobject$clustering,hcut2)

emclust$classification

plot(emclust)

library(cluster)
clusplot(df2,km.at.3$cluster)

km.at.26<-kmeans(df2,26,nstart=25)
saveRDS(km.at.26,'kmat26.rds')



clusplot(df2,km.at.26$cluster)



#install.packages('pvclust')
#library(pvclust)
#http://www.statmethods.net/advstats/cluster.html
#pvattempt<-pvclust(t(df2), method.hclust="ward", method.dist="euclidean")
#saveRDS(pvattempt,'pvattempt.RDS')


setwd("/Users/nugthug/Documents/cmpsci/348/Assignment/03")
library(fpc)
library(tidyverse)
df <- read.csv("isolet1+2+3+4.data")
colnames(df) <- c(paste("val_",1:(ncol(df)-1), sep=""), "letter")
df
df2<-select(df,-letter)

#reading in objects since models took a long time and R kept crashing.


pamkRun<-readRDS('pamkRun.rds')
wss<-readRDS('kmeans1to30.rds')
km.at.26<-readRDS('kmat26.rds')
km.at.2<-readRDS('kmat2.rds')
km.at.3<-readRDS('kmat3.rds')
emclust<-readRDS('emclust.RDS')
harch<-readRDS('harchComplete.rds')
pamkRunMultAsw<-readRDS('pamkRunMultAsw.rds')

emclust


hcut.at.3<-cutree(harch,3)

plot(harch)

library(cluster)
#plots for question 2
clusplot(df2,emclust$classification,color=TRUE,shade=TRUE,main="Mclust with 3 Clusters \n CLUSPLOT" )

clusplot(df2,hcut.at.3,color=TRUE,shade=TRUE,main="harch \n Cut at 3 \n CLUSPLOT")

clusplot(df2,km.at.3$cluster,color=TRUE,shade=TRUE, main="K-means \n k=3 \n CLUSPLOT")

clusplot(df2,km.at.26$cluster,color=TRUE,shade=TRUE, main="K-means \n k=26 \n CLUSPLOT")

#plots for question 3
df3<-select(df2,num_range('val_',1:50))

subset.km.at.3<-kmeans(df3,3,nstart=25)
saveRDS(subset.km.at.3,"subsetkm3.rds")
clusplot(df3,subset.km.at.3$cluster,color=TRUE,shade=TRUE, main="K-means Only 50 Variables \n k=3 \n CLUSPLOT")

subset.harch<-hclust(dist(df3),method='complete')
saveRDS(subset.harch,'subsetharc.rds')

#smaller variable count clust plot
clusplot(df3,subset.km.at.3$cluster,color=TRUE,shade=TRUE, main="K-means Only 50 Variables \n k=3 \n CLUSPLOT")
subset.hcut.at.3<-cutree(subset.harch,3)
clusplot(df3,subset.hcut.at.3,color=TRUE,shade=TRUE, main="Harch Only 50 Variables \n k=3 \n CLUSPLOT")


#testing to see if elbow has moved on reduced dataset
k.max<-30
wss2 <- sapply(1:k.max, function(k){kmeans(df3, k, nstart=25 )$tot.withinss})
#saveRDS(wss2,'kmeans1to30.rds')
plot(1:k.max, wss2,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="50 Variable Kmeans with varied K")
abline(v = 6, lty =2)





#plotting the kmeans clustering within cluster sum of squares over multiple k
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Within Cluster Sum of Squares over K")
abline(v = 3, lty =2)


library(mclust)
subset.emclust<-Mclust(df3)
subset.emclust
saveRDS(subset.emclust,'subsetemclust.RDS')
subset.emclust<-readRDS('subsetemclust.RDS')

clusplot(df3,subset.emclust$classification,color=TRUE,shade=TRUE, main="Mclust Only 50 Variables \n CLUSPLOT")


#cleaning km at 26 and matching to labels
km.at.26.with.letters<-as_tibble(km.at.26$cluster)
km.at.26.with.letters<-bind_cols(km.at.26.with.letters,select(df,letter))
km.at.26.with.letters$letter<-as.factor(km.at.26.with.letters$letter)
km.at.26.with.letters$value<-as.factor(km.at.26.with.letters$value)
table(km.at.26.with.letters)

remove(km.at.26.with.letters)

ggplot(km.at.26.with.letters, aes(x=value,y=letter,color=value))+geom_point()

install.packages('clusteval')

#For question 4, using cluster_similarity 
library(clusteval)
comembership_table(km.at.26.with.letters$value,km.at.26.with.letters$letter)
cluster_similarity(km.at.26.with.letters$value,km.at.26.with.letters$letter)


pamk.at.26<-pamk(df2,krange=26:26,criterion = 'multiasw',usepam=FALSE)
pamk.at.26.with.letters<-as_tibble(pamk.at.26$pamobject$clustering)

pamk.at.26.with.letters<-bind_cols(pamk.at.26.with.letters,select(df,letter))

remove(pamk.at.26.with.letters)

pamk.at.26.with.letters

cluster_similarity(pamk.at.26.with.letters$value,pamk.at.26.with.letters$letter)

harch.at.26<-cutree(harch,26)
harch.at

harch.at.26.with.letters<-as_tibble(harch.at.26)
harch.at.26.with.letters<-bind_cols(harch.at.26.with.letters,select(df,letter))
cluster_similarity(harch.at.26.with.letters$letter,harch.at.26.with.letters$value)

#question 5, contingency tables and chi square analysis
library(MASS)
km.at.3.harch.at.3<-table(km.at.3$cluster,hcut.at.3)
km.at.3.harch.at.3
chisq.test(km.at.3.harch.at.3)

chisq.test(table(km.at.3$cluster,km.at.2$cluster))

km.at.2.harch.at.3<-table(km.at.2$cluster,hcut.at.3)
chisq.test(km.at.2.harch.at.3)

km.at.3.emclust<-table(km.at.3$cluster,emclust$classification)
chisq.test(km.at.3.emclust)

km.at.3.emclust

km.at.2.emclust<-table(km.at.2$cluster,emclust$classification)
chisq.test(km.at.2.emclust)

emclust.harch.at.3<-table(hcut.at.3,emclust$classification)
chisq.test(emclust.harch.at.3)

