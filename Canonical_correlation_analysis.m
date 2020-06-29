%典型相关分析
%By Alessio-yuan 
%reference: blog https://www.cnblogs.com/duye/p/9384821.html

%[xcoeff,ycoeff,cor,x_c,y_c,stats] = canoncorr(x,y)
%% Input
%x：原始变量x矩阵，每列一个自变量指标，第i列是 xi 的样本值，行为样本
%y：原始变量y矩阵，每列一个因变量指标，第j列是 yj 的样本值，行为样本

%% Output
%xcoeff：自变量x的典型相关变量系数矩阵，每列是一组系数。列数为典型相关变量数。
%ycoeff: 因变量y的典型相关变量系数矩阵，每列是一个系数
%cor：   典型相关系数。即第一对<u1,v1>之间的相关系数、第二对<u2,v2>之间的相关系数。
%x_c：   对于X的计算所用的典型相关变量的值
%y_c:    对于Y的计算所用的典型相关变量的值
%stats:  假设检验量 使用chisq即Bartlett提出的卡方检验方法

%% data
[~,p] = size(x);
[~,q] = size(y);

%% analysis
[sample_size,dim] = size(x);
[xcoeff,ycoeff,cor,x_CCA,y_CCA,stats] = canoncorr(x,y);

%% result典型变量
X_CCA = x * xcoeff(:,1:dim);
Y_CCA = y * ycoeff(:,1:dim);

%% rest冗余分析
Rxv = zeros(p,dim); 
Rxw = zeros(p,dim); 
Ryw = zeros(q,dim); 
Ryv = zeros(q,dim); 
for j = 1:p
    for k = 1:dim
        Rxv(j,k) = corr(x(:,j),X_CCA(:,k))^2/p;
    end
end
for j = 1:p
    for k = 1:dim
        Rxw(j,k) = (cor(k)^2)*Rxv(j,k);
    end
end
for j = 1:q
    for k = 1:dim
        Ryw(j,k) = corr(y(:,j),Y_CCA(:,k))^2/q;
    end
end
for j = 1:q
    for k = 1:dim
        Ryv(j,k) = (cor(k)^2)*Ryw(j,k);
    end
end
xv = sum(sum(Rxv));%x的解释量
xw = sum(sum(Rxw));%x的冗余 
yw = sum(sum(Ryw));%y的解释量
yv = sum(sum(Ryv));%y的冗余

%% test
test = zeros(dim,1);
for k = 1:dim
    test(k) = (p-k+1)*(q-k+1);
    test(k) = chi2inv(1-alpha,test(k));
end
flag = 1; %表示拒绝原假设
rv_number = 0; %可提取维数
for k = 1:dim
    if stats.chisq(k) < test(k)
        flag = 0;
        rv_number = k-1;
        break
    end
end
if flag == 1
    disp("pass");
else
    disp(rv_number);
end
