# gof for btergm
# BTERGM

library(btergm)
library(ergm)
library(network)
library(sna)
setwd("E:/指数随机图/patentsview")

networks <- readRDS(file = "networks_all_1120.rds")
cpc_similarity3 <- read.csv("cpc_semanticsimilarity.csv") # 语义相似性

all_nodes <- unique(unlist(lapply(networks, network.vertex.names)))
semantic_similarity_matrix <- as.matrix(cpc_similarity3)


ptm <- proc.time()
set.seed(101)

model1.1 <- btergm(
  networks[1:9]
  ~ edges
  + gwesp(0.25, fixed = TRUE)
  ,R = 500,
  parallel = "snow",
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm

# 结果保存与读取
save(model1.1, file="model1.1.rda")

ptm <- proc.time()
set.seed(101)

model2.1 <- btergm(
  networks[1:9]
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

save(model2.1, file="model2.1.rda")


ptm <- proc.time()
set.seed(101)

model3.1 <- btergm(
  networks[1:9]
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

save(model3.1, file="model3.1.rda")



ptm <- proc.time()
set.seed(101)

model4.1 <- btergm(
  networks[1:9]
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

save(model4.1, file="model4.1.rda")



ptm <- proc.time()
set.seed(101)

model5.1 <- btergm(
  networks[1:9]
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

save(model5.1, file="model5.1.rda")



ptm <- proc.time()
set.seed(101)

model6.1 <- btergm(
  networks[1:9]
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

save(model6.1, file="model6.1.rda")


ptm <- proc.time()
set.seed(101)

model7.1 <- btergm(
  networks[1:9]
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

save(model7.1, file="model7.1.rda")


ptm <- proc.time()
set.seed(101)

model8.1 <- btergm(
  networks[1:9]
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

save(model8.1, file="model8.1.rda")


ptm <- proc.time()
set.seed(101)

model9.1 <- btergm(
  networks[1:9]
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

save(model9.1, file="model9.1.rda")


ptm <- proc.time()
set.seed(101)

model10.1 <- btergm(
  networks[1:9]
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

save(model10.1, file="model10.1.rda")



# GOF


load("model1.1.rda")
gof1 <- btergm::gof(model1.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
                    , coef = coef(model1.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof1, file = "gof.model1.rda")

load("model2.1.rda")
gof2 <- btergm::gof(model2.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
                    + nodecov("yearly_degree_log")
                    + absdiff("yearly_degree_log")
                    + nodecov("eigenvector_sqrt")
                    + absdiff("eigenvector_sqrt")
                    , coef = coef(model2.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof2, file = "gof.model2.rda")

load("model3.1.rda")
gof3 <- btergm::gof(model3.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
                    + nodematch("section")
                    + nodematch("class")
                    + nodecov("alt_sqrt")
                    + absdiff("alt_sqrt")
                    + nodecov("group_log")
                    + absdiff("group_log")
                    + edgecov(semantic_similarity_matrix)
                    , coef = coef(model3.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof3, file = "gof.model3.rda")

load("model4.1.rda")
gof4 <- btergm::gof(model4.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
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
                    , coef = coef(model4.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof4, file = "gof.model4.rda")

load("model5.1.rda")
gof5 <- btergm::gof(model5.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
                    + nodecov("funded_log")
                    + absdiff("funded_log")
                    + nodecov("log_economic_output")
                    + absdiff("log_economic_output")
                    , coef = coef(model5.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof5, file = "gof.model5.rda")


load("model6.1.rda")
gof6 <- btergm::gof(model6.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
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
                    , coef = coef(model6.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof6, file = "gof.model6.rda")

load("model7.1.rda")
gof7 <- btergm::gof(model7.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
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
                    + memory(type = "autoregression", lag = 1), coef = coef(model7.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof7, file = "gof.model7.rda")

load("model8.1.rda")
gof8 <- btergm::gof(model8.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
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
                    + memory(type = "stability", lag = 1), coef = coef(model8.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof8, file = "gof.model8.rda")

load("model9.1.rda")
gof9 <- btergm::gof(model9.1, nsim = 100, target = networks[[10]],
                    formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
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
                    + memory(type = "loss", lag = 1), coef = coef(model9.1),
                    statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof9, file = "gof.model9.rda")



load("model10.1.rda")
gof10 <- btergm::gof(model10.1, nsim = 100, target = networks[[10]],
                     formula = networks[9:10] ~ edges + gwesp(0.25, fixed = TRUE)
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
                     + memory(type = "innovation", lag = 1), coef = coef(model10.1),
                     statistics = c(esp, dsp, geodesic, deg, triad.undirected, rocpr))

save(gof10, file = "gof.model10.rda")


load("gof.model7.rda")
gof1[[6]]$auc.pr

pdf("gof.pdf", width = 8, height = 6)
plot(gof7, roc.rgraph = TRUE, pr.rgraph = TRUE)
dev.off()

# 也可以分开画

png("gof7_1.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(5, 5, 3, 1),  # 下、左、上、右边距
  cex.lab = 2)         # 坐标轴标签大小
plot(gof7[[1]], main = "", axes = FALSE)
axis(1, at = seq(0, 400, by = 40), cex.axis = 1.5) 
axis(2, at = seq(0, 0.01, by = 0.002), cex.axis = 1.5)
# 添加标题
title(main = "Edge-wise shared partners", xlab = "Edge-wise shared partners", ylab = "Frequency", cex.main = 2.5)
dev.off()

png("gof7_2.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(5, 5, 3, 1),  # 下、左、上、右边距
    cex.lab = 2)         # 坐标轴标签大小
plot(gof7[[2]], main = "", axes = FALSE)
axis(1, at = seq(0, 400, by = 40), cex.axis = 1.5) 
axis(2, at = seq(0, 0.08, by = 0.02), cex.axis = 1.5)
# 添加标题
title(main = "Dyad-wise shared partners", xlab = "Dyad-wise shared partners", ylab = "Frequency", cex.main = 2.5)
dev.off()

png("gof7_3.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(5, 5, 3, 1),  # 下、左、上、右边距
    cex.lab = 2)         # 坐标轴标签大小
plot(gof7[[3]], main = "", axes = FALSE, xlim = 6)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c("1", "2", "3", "4", "5", ">5"), cex.axis = 1.5) 
axis(2, at = seq(0, 0.8, by = 0.2), cex.axis = 1.5)
# 添加标题
title(main = "Geodesic distances", xlab = "Geodesic distances", ylab = "Frequency", cex.main = 2.5)
dev.off()

png("gof7_4.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(5, 5, 3, 1),  # 下、左、上、右边距
    cex.lab = 2)         # 坐标轴标签大小
plot(gof7[[4]], main = "", axes = FALSE)
axis(1, at = seq(0, 500, by = 50), cex.axis = 1.5) 
axis(2, at = seq(0, 0.02, by = 0.005), cex.axis = 1.5)
# 添加标题
title(main = "Degree", xlab = "Degree", ylab = "Frequency", cex.main = 2.5)
dev.off()

png("gof7_5.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(5, 5, 3, 1),  # 下、左、上、右边距
    cex.lab = 2)         # 坐标轴标签大小
plot(gof7[[5]], main = "", axes = FALSE, xlim = 4)
axis(1, at = c(1, 2, 3, 4), labels = c("0", "1", "2", "3"), cex.axis = 1.5) 
axis(2, at = seq(0, 0.6, by = 0.1), cex.axis = 1.5)
# 添加标题
title(main = "Triad census", xlab = "Triad census", ylab = "Frequency", cex.main = 2.5)
dev.off()

png("gof7_6.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(5, 5, 3, 1),  # 下、左、上、右边距
    cex.axis = 1.5,      # 坐标轴刻度大小
    cex.lab = 2)         # 坐标轴标签大小
plot(gof7[[6]], main = "", axes = FALSE)
# 添加标题
title(main = "ROCPR", xlab = "", ylab = "", cex.main = 2.5)
dev.off()
