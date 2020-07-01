################ package ################
library(psych)
library(vegan)
mashi <-function(a,b){
#a,b均为向量
return (((a-b)%*% t(t(a-b))) / cov(a,b))
}

#dist(data, method = "euclidean", diag = FALSE, upper = FALSE, p=2)
################ Input #################
#data:每一行为要计算距离的向量
#diag 添加对角线 upper添加上半三角
#p 闵式距离的维数
################ Output ################
#输出为行向量两两组合的距离矩阵

#hclust(distance, method = "complete", members = NULL)
################ Input #################
#distance: 距离矩阵
#method: 聚类使用的方法：本题中使用的重心法
################ Output ################
#cluster类

#kmeans(x,centers)
################ Input #################
#x:行为待聚类样本，列为聚类指标
#centers: 聚类个数
################ Output ################
#size:两个聚类中的元素个数
#iter:未进入聚类的个体数
#withinss:集群内的方差之和 tot.withinss 全体聚类内的平方和 totss总平方和 
#center:聚类中心坐标
#cluster：聚类序号

################ Example ###############
if (FALSE){
  Y=iris[c(1:10),]
  distance<-dist(Y[,-5],"euclidean", diag = TRUE)
  iris.hc<-hclust(distance, method = "centroid")
  plot(iris.hc,hang=-1)
  result1=rect.hclust(iris.hc,k=5,border="red")
  x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
             matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
  colnames(x) <- c("x", "y")
  cl <- kmeans(x, 2)
  plot(x, col = cl$cluster)
  points(cl$centers, col = 1:2, pch = 8, cex = 2)
}

################ Data ###################
#data
#cluster.num

################ Analysis ###############
Kmeans.Cluster.result = kmeans(data,cluster.num)
distance = dist(data,"euclidean", diag = TRUE)
center.cluster.result = hclust(distance, method = "centroid")
h.Cluster.result = rect.hclust(center.cluster.result, k = cluster.num, border="red")

################ Result #################
Cluster.name = Kmeans.Cluster.result$cluster
Cluster.center = Kmeans.Cluster.result$centers
Cluster.var = Kmeans.Cluster.result$withinss
out.size = Kmeans.Cluster.result$iter
hcluser.name = h.Cluster.result #as.numeric(h.Cluster.result[[i]])返回第i个聚类中元素名称

################ Vision #################
plot(data, col = Cluster.name)
points(Cluster.center, col = 1:2, pch = 8, cex = 2)
