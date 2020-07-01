############### Package ##############
library(psych)
library(GPArotation)
library(ggplot2)
library(MASS)
source("data_preprocess.r")

#主成分分析
#By Alessio-yuan

#prcomp(data,retx = TRUE, center = TRUE, scale. = FALSE, rank. = NULL)

############### Input ################
#data   变量：行为样本、列为因子
#retx   是否返回旋转变量
#center 均值归0化，scale方差归1化
#rank   主成分最大数量，在主成分数量远小于矩阵维数时有用

############### Output ###############
#scale  每一个变量的方差 center 均值
#rotation 主成分系数
#x        主成分输出(在不标准化下右乘rot等于原数)
#sdev     主成分标准差（奇异值开方） sqrt(lambda_i)
#prop of var 方差占比

############### Example ##############
if (FALSE){
  ## signs are random
  require(graphics)
  ## the variances of the variables in the
  ## USArrests data vary by orders of magnitude, so scaling is appropriate
  prcomp(USArrests)  # inappropriate
  prcomp(USArrests, scale = TRUE)
  prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
  plot(prcomp(USArrests))
  summary(prcomp(USArrests, scale = TRUE))
  biplot(prcomp(USArrests, scale = TRUE))
}

############### Data #################
#data

############### Analysis #############
PCA.result = prcomp(data,retx = TRUE, center = FALSE, scale. = FALSE, rank. = NULL)
cor.result = cor(data)

############### Result ###############
data.scale = PCA.result$scale
data.center = PCA.result$center #样本均值与方差
PCA.data = PCA.result$x     #主成分样本数据
PCA.rotation = PCA.result$rotation #主成分系数矩阵
PCA.var = PCA.result$sdev #主成分的方差值

############### Vision ###############
#arrow箭头表示主成分占比
xlab <- paste("PC1","(",round((summary(PCA.result))$importance[2,1]*100,1),"%)",sep="")
ylab <- paste("PC2","(",round((summary(PCA.result))$importance[2,2]*100,1),"%)",sep="")
x<-"PC1"
y<-"PC2"
data_x <- data.frame(varnames=rownames(PCA.result$x), PCA.result$x) #为方便取用数据，将pca结果放在一个数据库里面
plot_1 <- ggplot(data_x, aes(PC1,PC2))+geom_point(aes(color=varnames),size=3)+coord_equal(ratio=1)+xlab(xlab)+ylab(ylab) # 这里先画出点，coord_equal(ratio=1) 将X轴和y轴比例设置为一样的
data_rotation <- data.frame(obsnames=row.names(PCA.result$rotation), PCA.result$rotation)
mult <- min((max(data_x[,y]) - min(data_x[,y])/(max(data_rotation[,y])-min(data_rotation[,y]))),(max(data_x[,x]) - min(data_x[,x])/(max(data_rotation[,x])-min(data_rotation[,x]))))#获取箭头缩放比例
data_2 <- transform(data_rotation,v1 = mult * (get(x)),v2 = mult * (get(y)))#设置箭头坐标
plot_1<-plot_1+geom_segment(data=data_2,aes(x=0,y=0,xend=v1,yend=v2),arrow=arrow(length=unit(0.2,"cm")),alpha=0.75)#添加箭头
plot_1<-plot_1+geom_text(data=data_2,aes(v1,v2,label=obsnames),size=3,nudge_x=-0.05,nudge_y=-0.01)#添加箭头名称  
plot_1 <- plot_1+scale_color_discrete(guide=guide_legend(title="stage type"))+theme_bw()+theme(plot.background=element_blank(),panel.background=element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),axis.title=element_text(color="black",size=15),axis.text=element_text(size=15))+guides(color=F) #对图形结果进行修饰
ggsave("PCA_example.png",plot=plot_1,device = "tiff",dpi=300,width = 6.5,height = 5.5) #绘图保存

#置信椭圆 适合样本分类的情形，可以在同类样本中绘制椭圆
data_x[, "varnames"] = "a"
data_x[1:10,"varnames"] = "b"
data_x[11:20,"varnames"] = "c"
ggplot(data_x,aes(x=PC1,y=PC2,color=varnames))+ geom_point()
ggplot(data_x,aes(x=PC1,y=PC2,color=varnames)) + geom_point()+ theme_bw() + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line= element_line(colour = "black")) #去掉背景网格版本
percentage<-round(PCA.result$sdev / sum(PCA.result$sdev) * 100,2)
percentage<-paste(colnames(data_x),"(", paste(as.character(percentage), "%", ")", sep=""))
ggplot(data_x,aes(x=PC1,y=PC2,color = varnames)) + geom_point() + xlab(percentage[1]) + ylab(percentage[2])#添加百分比
plot_2 = ggplot(data_x,aes(x=PC1,y=PC2,color = varnames)) + geom_point() + stat_ellipse(level = 0.95, show.legend = F) + 
  annotate('text', label = NULL, x = -2, y = -1.25, size = 5, colour = '#f8766d') +
  annotate('text', label = NULL, x = 0, y = - 0.5, size = 5, colour = '#00ba38') +
  annotate('text', label = NULL, x = 3, y = 0.5, size = 5, colour = '#619cff')

#####################################################################################

#因子分析
#By Alessio-yuan

#factanal(corr, factors, subset, na.action, scores = "Bartlett",rotation = "varimax")

############### Input ################
#corr 相关系数矩阵（对按列标准化变量来讲，与协方差矩阵相同，如果为标化，为协方差阵除以对应列方差根号积）
#factors因子个数
#scores = c("none", "regression", "Bartlett")因子得分：reg表示汤普森，常用巴特莱特
#rotation = "varimax"方差最大化因子旋转
#na.action在corr用作公式时开启
#subset尚不清楚
#通常只用factanal(corr, factors, scores = "Bartlett",rotation = "varimax")

############### Output ###############
#loadings 载荷矩阵
#Factor Correlations 因子相关矩阵
#Cumulative Var累计方差 Proportion Var占比方差
#Uniquenesses 计算唯一性（尚未搞定）

############### example ##############
if (FALSE){
  v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
  v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
  v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
  v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
  v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
  v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
  m1 <- cbind(v1,v2,v3,v4,v5,v6)
  cor(m1)
  factanal(m1, factors = 3)
  factanal(m1, factors = 3, rotation = "promax")
  prcomp(m1) 
  factanal(~v1+v2+v3+v4+v5+v6, factors = 3,scores = "Bartlett")$scores
}

############### data ################
#data    #数据框 列为待合并的变量数，行为样本数
factor.num = 3    #待提取的因子个数

############### Analysis ############
EFA.result = factanal(data, factors = factor.num, scores = "Bartlett",rotation = "varimax")
cor.result = cor(data) #变量相关性矩阵
FA.result = fa(cor.result, nfactors = factor.num, rotate = "varimax", fm="pa")#斜交因子旋转

############### Result ##############
loadings = EFA.result$loadings #载荷+方差解释量
loading = loadings[,1:factor.num] #载荷
rotation.matrix = EFA.result$rotmat
strange.Var = EFA.result$uniquenesses #特殊方差
scores = solve(t(loading) %*% solve(diag(strange.Var)) %*% loading) %*% t(loading) %*% solve(diag(strange.Var)) %*% t(as.matrix(data))#巴特莱特因子得分得

############### Vision ###############
fa.diagram(FA.result,simple=TRUE)
factor.plot(FA.result,labels=rownames(FA.result$loadings))





#补充：对应分析
#corresp(data,nf=1)
############### Input ################
#data 数据矩阵：行为样本，列为特征
#nf 提取的对应因子数

############### Output ###############
#行列坐标

############### Example ##############
if (FALSE){
  corresp(caith)
  biplot(corresp(caith, nf = 2))
}

############### Data #################
#data

############### Analysis #############
Corr.result = corresp(data,nf=2)

############### Result ###############
row.point = Corr.result$rscore
col.point = Corr.result$cscore
lambda = Corr.result$cor

############### Test #################
m = nrow(data)
n = ncol(data)
Z_data = standard.prop(data)
total = Z_data$total
row.margin = Z_data$row.margin
row.margin = row.margin/total
col.margin = Z_data$col.margin
col.margin = col.margin/total
data = Z_data$data
Z_data = data
for (i in c(1:m)){
  for (j in c(1:n)){
    Z_data[i,j] = (data[i,j]-row.margin[i]*col.margin[j])/sqrt(row.margin[i]*col.margin[j])
  }
}
test = eigen(t(Z_data)%*%Z_data)
lambda = test$values
test.var = total*sum(lambda)
if (test.var>qchisq(0.95, m*n)){
  print("refuse")
} else{
  print("pass")
}







#Question : how to get a picture with arrow+circle
