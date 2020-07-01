%By Alessio-yuan
%主成分分析

%[coef,score,latent] = princomp(data)
%% Input
%data:M*N维矩阵，行为样本，列为特征

%% Output
%coef:系数矩阵，第i列为第i个主成分系数
%score: 主成分的样本观测值
%latent:奇异值

%% Data
[~,n] = size(data);
data = table2array(data);
alpha = 0.05;

%% Analysis
[coef,score,latent,t2] = pca(data);
pca_score = data*coef;

%% test
ndim=barttest(data,alpha);

%因子分析
%[lambda,psi,T,stats,F] = factoran(data,factor_num,'score','Bartlett');
%% Input 
%data: 行为样本，列为特征
%factor_num: 因子数量

%% Output
%lambda: 因子载荷矩阵
%psi: 特殊方差的最大似然估计值
%T: 因子旋转矩阵
%stats: p为卡方检验的概率值

%% Data
[~,m] = size(data);
data = table2array(data);
factor_num = 2;

%% Analysis
[lambda,psi,T,stats,F] = factoran(data,factor_num,'score','Bartlett');

%% Result
contribut = 100*sum(lambda.^2)/m;  %因子贡献量
scores = inv(lambda'*inv(diag(psi))*lambda)*lambda'*inv(diag(psi))*data'; %巴特莱特因子得分
