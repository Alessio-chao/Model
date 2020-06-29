#典型相关分析
#By Alessio-yuan

############ Input ##############
#cancor(x, y, xcenter = TRUE, ycenter = TRUE)
#x,y即为R.V.组格式为：行样本，列随机变量指标
#xc,yc 如果设置为TRUE会减去样本均值（感觉还是预先按照要求标准化

############ Output ############
#cor 即为典型变量的相关系数lambda_i #cor(x%*%xcoef,y%*%ycoef)
#xcoef 第i列为第i个典型变量的系数向量 X[1,]%*%xcoef[,i]
#xcenter 原数据矩阵第i列指标的样本均值mean(X[,i])
#注明：这里较高维度的RV输出行需手动取前p列

############ example ################
if (FALSE){
  help(cancor)
  pop <- LifeCycleSavings[, 2:3]
  oec <- LifeCycleSavings[, -(2:3)]
  result = cancor(pop, oec)
  x <- matrix(rnorm(150), 50, 3)
  y <- matrix(rnorm(250), 50, 5)
  (cxy <- cancor(x, y))
  all(abs(cor(x %*% cxy$xcoef,
              y %*% cxy$ycoef)[,1:3] - diag(cxy $ cor)) < 1e-15)
  all(abs(cor(x %*% cxy$xcoef) - diag(3)) < 1e-15)
  all(abs(cor(y %*% cxy$ycoef) - diag(5)) < 1e-15)
}

############ data ###############
X = as.matrix(X)
Y = as.matrix(Y)

############ Analysis ###########
dim = min(ncol(X), ncol(Y))
sample.num = nrow(X)
p = ncol(X)
q = ncol(Y)
CCAresult = cancor(X, Y, xcenter = TRUE, ycenter = TRUE)

############ Result #############典型变量
X.sample_mean = CCAresult$xcenter
y.sample_mean = CCAresult$ycenter
X_CCA = X %*% CCAresult$xcoef[,1:dim] #第k列即为第k个典型变量
Y_CCA = Y %*% CCAresult$ycoef[,1:dim]
lambda = CCAresult$cor

############ Rest ###############冗余分析
Rxv = matrix(rep(0,p*dim),nrow = p)
Rxw = matrix(rep(0,p*dim),nrow = p)
Ryw = matrix(rep(0,q*dim),nrow = q)
Ryv = matrix(rep(0,q*dim),nrow = q)
for (j in c(1:p)){
  for (k in c(1:dim)){
    Rxv[j,k] = cor(X[,j],X_CCA[,k])^2/p;
  }
}
for (j in c(1:p)){
  for (k in c(1:dim)){
    Rxw[j,k] = (lambda[k]^2)*Rxv[j,k];
  }
}
for (j in c(1:q)){
  for (k in c(1:dim)){
    Ryw[j,k] = cor(Y[,j],Y_CCA[,k])^2/q;
  }
}
for (j in c(1:q)){
  for (k in c(1:dim)){
    Ryv[j,k] = (lambda[k]^2)*Ryw[j,k];
  }
}
Rxv = data.frame(Rxv)
colnames(Rxv) = c(1:dim) #一定注意tapply使用前的第二个向量是列名
xv = tapply(Rxv, c(1:dim), sum) 
xv = sum(xv)#X的解释量
xw = sum(apply(Rxw, 1, sum)) #X的冗余
yw = sum(apply(Ryw, 2, sum)) #Y的解释量
yv = sum(apply(Ryv, 1, sum)) #Y的冗余

############ Test ###############
lambda = 1-lambda^2
lambda = log(lambda)
qk = rep(0,dim)
for (k in c(1:dim)){
  qk[k] = sum(lambda[k:dim])
  qk[k] = -(sample.num-k-(p+q+1)/2)*qk[k]
}
test = rep(0,dim)
for (k in c(1:dim)){
  test[k] = (p-k+1)*(q-k+1)
  test[k] = qchisq(0.95, test[k])
}
flag = 1 #表示拒绝原假设
rv_number = 0 #可提取维数
for (k in c(1:dim)){
  if (qk[k]<test[k]){
    flag = 0
    rv_number = k-1
    break
  }
}
if (flag == 1){
  print("pass")
}else{
  print(rv_number)
}
