%聚类分析
%By Alessio-yuan

%[index,center,sum_square,dist] = kmeans(data,k,'Distance','sqEuclidean','Start',','sample','Replicates',2)
%% Input
%data: 行为样本、列为指标的数据
%k:    目标聚类个数
%Distance-sqEuclidean  计算距离的方法-欧氏距离
%Start 聚类起始点选取方法-sample随机抽样
%Replicates 重复聚类取均值次数

%% Output
%index: 聚类的序号
%center: 第i行为第i类的
%sum_square: 样本到中心的距离总和
%dist: 每一个样本到中心的距离

%% Data
[m,n] = size(data);
data = table2array(data);

%% Analysis
[index,center,sum_square,dist] = kmeans(data,k,'Distance','sqEuclidean','Start','sample','Replicates',2);

%% 系统聚类
distance = pdist(data);
tree = linkage(distance,'average');%类平均法 centroid重心法
[figure_tree,t] = dendrogram(tree);
title('系统聚类树状图');
set(figure_tree,'linewidth',2);
ylabel('标准距离');
hcluster_result = cluster(tree,'maxclust',3);
