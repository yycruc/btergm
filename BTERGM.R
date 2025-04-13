# BTERGM
library(tidyverse)
library(statnet)
library(GGally)
library(ggplot2)
library(grid)
library(gridExtra)
library(Hmisc)
library(zoo)
library(cshapes)
library(patchwork)
library(btergm)
library(ergm)
library(speedglm)
library(ergMargins)
library(texreg)
library(igraph)
library(network)
library(lme4)
library(plm)
library(dplyr)
setwd("E:/指数随机图/patentsview")

networks <- readRDS(file = "networks_all_1120.rds")
all_nodes <- unique(unlist(lapply(networks, network.vertex.names)))
semantic_similarity_matrix <- read.csv("cpc_semanticsimilarity.csv") # 语义相似性
semantic_similarity_matrix <- as.matrix(semantic_similarity_matrix)

# 检查输入节点属性
#summary(networks[[1]])

ptm <- proc.time()
set.seed(101)

model1 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model1 <- summary(model1)
# 结果保存与读取
save(model1, file="model1.rda")

ptm <- proc.time()
set.seed(101)

model2 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model2 <- summary(model2)

save(model2, file="model2.rda")


ptm <- proc.time()
set.seed(101)

model3 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model3 <- summary(model3)

save(model3, file="model3.rda")


ptm <- proc.time()
set.seed(101)

model4 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model4 <- summary(model4)

save(model4, file="model4.rda")


ptm <- proc.time()
set.seed(101)

model5 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model5 <- summary(model5)

save(model5, file="model5.rda")


ptm <- proc.time()
set.seed(101)

model6 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model6 <- summary(model6)

save(model6, file="model6.rda")


ptm <- proc.time()
set.seed(101)

model7 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "autoregression", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model7 <- summary(model7)

save(model7, file="model7.rda")

#edge7 <- edgeprob(model7, verbose = FALSE)

## 另一种方法是将计数转换为概率
#future_model7 <- simulate(model7, nsim = 100)
#n_nodes <- network.size(future_model7[[1]])  # 获取节点数量
#edge_prob_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
#for (i in seq_along(future_model7)) {
#  adj_matrix <- as.matrix(future_model7[[i]])
#  edge_prob_matrix <- edge_prob_matrix + adj_matrix
#}
#edge_prob_matrix <- edge_prob_matrix / length(future_model7)
#write.csv(edge_prob_matrix,file="model7_predicted.csv")

ptm <- proc.time()
set.seed(101)

model8 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "stability", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model8 <- summary(model8)

save(model8, file="model8.rda")

ptm <- proc.time()
set.seed(101)

model9 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "loss", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model9 <- summary(model9)

save(model9, file="model9.rda")


ptm <- proc.time()
set.seed(101)

model10 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "innovation", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model10 <- summary(model10)

save(model10, file="model10.rda")


# 第二个模型
networks <- readRDS(file = "networks_25_1120.rds")
all_nodes <- unique(unlist(lapply(networks, network.vertex.names))) # 597
semantic_similarity_matrix <- read.csv("cpc_semanticsimilarity.csv") # 语义相似性
semantic_similarity_matrix <- as.matrix(semantic_similarity_matrix)

# 模型时间
ptm <- proc.time()
set.seed(101)

model11 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "autoregression", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model11 <- summary(model11)

# 结果保存与读取
save(model11, file="model11.rda")
load("model11.rda")

#future_model11 <- simulate(model11, nsim = 1)
#write.csv(as.matrix(future_model11),file="model11_predicted.csv")

# 将模型2存为rda文件

rm(networks)

# 第三个模型
networks <- readRDS(file = "networks_50_1120.rds")
all_nodes <- unique(unlist(lapply(networks, network.vertex.names))) # 597
semantic_similarity_matrix <- read.csv("cpc_semanticsimilarity.csv") # 语义相似性
semantic_similarity_matrix <- as.matrix(semantic_similarity_matrix)

# 模型时间
ptm <- proc.time()
set.seed(101)

model12 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "autoregression", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model12 <- summary(model12)

# 结果保存与读取
save(model12, file="model12.rda")
load("model12.rda")

#future_model12 <- simulate(model12, nsim = 1)
#write.csv(as.matrix(future_model12),file="model12_predicted.csv")

rm(networks)


# 第四个模型
networks <- readRDS(file = "networks_75_1120.rds")
all_nodes <- unique(unlist(lapply(networks, network.vertex.names))) # 597
semantic_similarity_matrix <- read.csv("cpc_semanticsimilarity.csv") # 语义相似性
semantic_similarity_matrix <- as.matrix(semantic_similarity_matrix)

# 模型时间
ptm <- proc.time()
set.seed(101)

model13 <- btergm(
  networks
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  + nodecov("yearly_degree_log")
  + absdiff("yearly_degree_log")
  + nodecov("eigenvector_sqrt")
  + absdiff("eigenvector_sqrt")
  + nodematch("section")
  + nodematch("class")
  + nodecov("alt_sqrt")
  + absdiff("alt_sqrt")
  + nodecov("group_log")
  + absdiff("group_log")
  + edgecov(semantic_similarity_matrix)
  + nodecov("funded_log")
  + absdiff("funded_log")
  + nodecov("log_economic_output")
  + absdiff("log_economic_output")
  + memory(type = "autoregression", lag = 1)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm
summary_model13 <- summary(model13)

# 结果保存与读取
save(model13, file="model13.rda")
load("model13.rda")

#future_model13 <- simulate(model13, nsim = 1)
#write.csv(as.matrix(future_model13),file="model13_predicted.csv")

# 将模型3存为rda文件


htmlreg(list(model1,model2,model3,model4,model5,model6), digits = 3, single.row = F,file = "btergm_1_6.doc")
htmlreg(list(model7,model11,model12,model13), digits = 3, single.row = F,file = "btergm_7_13.doc")




btergm1 <- summary(model7)
btergm2 <- summary(model11)
btergm3 <- summary(model12)
btergm4 <- summary(model13)

# 多重共线性
vif.ergm<-function(my.ergm){
  
  
  if(class(my.ergm)%in%"btergm"){
    data_mat<-my.ergm@effects
    corr5<-stats::cor(data_mat[!rownames(data_mat)%in%"edges",
                               !colnames(data_mat)%in%"edges"]) ## 排除边
    beta<-btergm::coef(my.ergm)
  }else{
    
    # 相关性矩阵
    if(class(my.ergm)%in%"mlergm"){
      cor.mat<-stats::cov2cor(solve(my.ergm$information_matrix))
      beta<-my.ergm$theta
      
    }else{
      cor.mat<-stats::cov2cor(my.ergm$covar) # 计算相关性
      beta<-stats::coef(my.ergm)
      
    }
    
    # 命名并去掉名为 "edges" 的行和列
    rownames(cor.mat)<-colnames(cor.mat)<-names(beta)
    corr5<-cor.mat[!rownames(cor.mat)%in%"edges",
                   !colnames(cor.mat)%in%"edges"]
  }
  
  # 去掉缺失值
  corr5<-corr5[!is.na(corr5[1:nrow(corr5)]),]
  corr5<-corr5[,which(!is.na(corr5[1,1:ncol(corr5)]))]
  
  VIFS<-matrix(0,nrow=1,ncol=ncol(corr5))
  
  # 循环计算每个自变量的 VIF 值
  for(i in 1:ncol(corr5)){
    
    gvec<-as.vector(corr5[-c(i),i]) ## 创建一个向量，包含目标自变量与其他自变量之间的相关性
    tgvec<-t(gvec) ## 转置该向量
    xcor<-solve(corr5[-c(i),-c(i)]) ## 创建一个只包含其他自变量的相关性矩阵，并求逆
    Rsq<-tgvec%*%xcor%*%gvec ## 计算目标自变量的 R^2
    VIFS[1,i]<-1/(1-Rsq) ## 根据 R^2 计算 VIF
  }
  
  colnames(VIFS)<-names(beta[!names(beta)%in%"edges"])
  
  if(class(my.ergm)%in%"btergm"){
    
    warning("VIFS for bootstrap TERGM based on model matrix, not the covariance matrix of the estimator. Benchmarks used for MCMC ML estimation may not apply.")
    
  }else{
    message("Higher values indicate greater correlation.\nVIF > 20 is concerning, VIF > 100 indicates severe collinearity.")
    
  }
  VIFS
}

vif.ergm(model7)
vif.ergm(model11)
vif.ergm(model12)
vif.ergm(model13)
