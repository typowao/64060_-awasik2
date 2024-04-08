library(dplyr)
library(cluster)
library(readr)
library(dendextend)
library(knitr)
library(factoextra)
#import data
data <- read.csv('C:/Users/ola/Documents/Spring24_MachineLearning/Cereals.csv', header =TRUE)
type <- str(data)

data <- na.omit(data)
head(data)

datachoose <- data.frame(data[,4:16])
data_norm <- scale(datachoose)

eu_dist <- dist(data_norm, method ="euclidean")

comp <- hclust(eu_dist, method = "complete")
plot(comp, cex = 0.6, hang = -1)

#agnes
hc_single <- agnes(data_norm, method = "single")
hc_complete <- agnes(data_norm, method = "complete")
hc_avg <- agnes(data_norm, method = "average")
hc_ward <- agnes(data_norm, method = "ward")

print(hc_single$ac)
print(hc_complete$ac)
print(hc_avg$ac)
print(hc_ward$ac)

#the best method -  Ward, because of the highest value 0.9046042

pltree(hc_ward, cex = 0.6, hang = -1)
rect.hclust(hc_ward, k=4, border = 1:4)
rect.hclust(hc_ward, k=5, border = 1:4)

# five clusters
clust <- cutree(hc_ward, k=5)
clust1 <- as.data.frame(cbind(data_norm, clust))

aa <- data_norm[1:23,]
bb <- data_norm[24:74,]
aa_norm <-scale(aa)
bb_norm <-scale(bb)
hc_s <- agnes(aa_norm, method = "single")
hc_c <- agnes(aa_norm, method = "complete")
hc_a <- agnes(aa_norm, method = "average")
hc_w <- agnes(aa_norm, method = "ward")

cbind(single = hc_s$ac, complete = hc_c$ac, average = hc_a$ac, ward = hc_w$ac)
pltree(hc_w, cex = 0.6, hang = -1)
rect.hclust(hc_w, k=5, border = 1:4)

clust2 <- cutree(hc_w, k=5)
cent <- as.data.frame(cbind(aa, clust2))
cent[cent$clust2 ==1,]

cent1 <- colMeans(cent[cent$clust2==1,])
cent[cent$clust2 ==2,]

cent2 <- colMeans(cent[cent$clust2==2,])
cent[cent$clust2 ==3,]

cent3 <- colMeans(cent[cent$clust2==3,])
cent[cent$clust2 ==4,]

cent4 <- colMeans(cent[cent$clust2==4,])
c <- rbind(cent1, cent2, cent3, cent4)
c2 <- as.data.frame(rbind(cent[,-14], bb))

diss <- get_dist(c2)
m <- as.matrix(diss)
diss1 <-data.frame(data=seq(1, nrow(bb), 1), clusters = rep(0, nrow(bb)))
for(i in 1:nrow(bb))
{diss1[i,2] <- which.min(m[i+4, 1:4])}
diss1

cbind(clust1$clust[24:74], diss1$clusters)
table(clust1$clust[24:74] == diss1$clusters)

# i think the model is not stable, because i got more false than true

he <- data
he <- na.omit(he)
he1 <- cbind(he, clust)
he1[he1$clust==1,]
he1[he1$clust==2,]
he1[he1$clust==3,]
he1[he1$clust==4,]

mean(he1[he1$clust==1,"rating"])
mean(he1[he1$clust==2,"rating"])
mean(he1[he1$clust==3,"rating"])
mean(he1[he1$clust==4,"rating"])

#i am choosing the cluster 1, because of the highest value as a cluster of “healthy cereals.”



