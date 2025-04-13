setwd("E:/指数随机图/patentsview")
library(igraph)
library(ggplot2)
library(network)
library(sna)
library(btergm)
library(backbone)
library(dplyr)

read_and_process_matrix <- function(year) {
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  # 读取igraph对象
  g <- readRDS(file_name)
  # 获取边权重
  edge_weights <- E(g)$weight
  # 转换为邻接矩阵
  matrix_adj <- as_adjacency_matrix(g, sparse = FALSE)
  matrix_net <- as.matrix(matrix_adj)
  # 使用 edge_weights 作为权重
  net <- as.network(
    matrix_net,
    matrix.type = "adjacency",
    ignore.eval = FALSE,   # 不忽略边权重
    names.eval = "weight",  # 指定边权重的名称
    directed = FALSE
  )
  # 添加边权重
  set.edge.attribute(net, "weight", value = edge_weights)
  return(net)
}

#read_and_process_matrix <- function(year) {
  #file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  ## 读取igraph对象
  #g <- readRDS(file_name)
  ## 获取边权重
  #edge_weights <- E(g)$weight
  # 转换为邻接矩阵
  #matrix_adj <- as_adjacency_matrix(g, sparse = FALSE)
  #matrix_net <-
    #disparity(matrix_adj,
    #          alpha = 0.37,
    #         narrative = TRUE,
    #          class = "Matrix") %>% as.matrix()
  ## 使用 edge_weights 作为权重
  #net <- as.network(
    #matrix_net,
    #matrix.type = "adjacency",
    #ignore.eval = FALSE,   # 不忽略边权重
    #names.eval = "weight",  # 指定边权重的名称
    #directed = FALSE
  #)
  ## 添加边权重
  #set.edge.attribute(net, "weight", value = edge_weights)
  #return(net)
#}

#read_and_process_matrix <- function(year, top_percent = 0.5) {
  #file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  ## 读取igraph对象
  #g <- readRDS(file_name)
  ## 获取边权重
  #edge_weights <- E(g)$weight
  ## 按照降序保留权重前 top_percent 的边
  #threshold <- quantile(edge_weights, probs = 1 - top_percent)
  ## 删除权重小于阈值的边
  #g <- delete_edges(g, E(g)[weight < threshold])
  ## 转换为邻接矩阵
  #matrix_adj <- as_adjacency_matrix(g, sparse = FALSE)
  
  ## 转换为 network 对象
  #net <- as.network(
    #matrix_adj,
    #matrix.type = "adjacency",
    #ignore.eval = FALSE,   # 不忽略边权重
    #names.eval = "weight",  # 指定边权重的名称
    #directed = FALSE
  #)
  ## 添加边权重
  #set.edge.attribute(net, "weight", value = E(g)$weight)
  #return(net)
#}

g <- readRDS("cpc_cooccurrence_network_2015.rds")
edge_weights <- E(g)$weight
summary(edge_weights)  # 计算均值、中位数、最小值、最大值、分位数等
quantile(edge_weights, probs = seq(0, 1, 0.1))  # 分位数
log_weights <- log1p(edge_weights)  # log(x+1) 避免 log(0) 错误
hist(log_weights, 
     breaks = 50, 
     main = "Log-transformed Edge Weight Distribution", 
     xlab = "Log(Edge Weight + 1)")
plot(ecdf(edge_weights), main = "Cumulative Distribution of Edge Weights", 
     xlab = "Edge Weight", ylab = "Cumulative Probability")

upper_threshold <- quantile(edge_weights, probs = 0.25)  # 上位5%
global_backbone <- global(
  G = g,
  upper = upper_threshold,
  class = "igraph",
  narrative = TRUE
)
cat("原网络边数:", gsize(g), "\n")
cat("主干网络边数:", gsize(global_backbone), "\n")

# global方法会自动去除边权
read_and_process_matrix <- function(year) {
  # 读取网络数据
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  g <- readRDS(file_name)
  edge_weights <- E(g)$weight
  upper_threshold <- quantile(edge_weights, probs = 0.99)
  
  # 使用 global 方法进行主干化
  matrix_net <- global(
    G = g,
    upper = upper_threshold,  # 指定上限阈值
    keepzeros = FALSE,        # 去除零权重边
    class = "Matrix",         # 返回稀疏矩阵格式
    narrative = TRUE          # 显示方法介绍
  ) %>% as.matrix()
  
  # 转换为 network 对象
  as.network(
    matrix_net,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight",
    directed = FALSE
  )
}

# 对多个年份的数据进行处理
years <- 2015:2019
networks <- lapply(years, read_and_process_matrix)

for (i in 1:length(networks)) {
  g <- intergraph::asIgraph(networks[[i]])
  print(gsize(g))
}

# 可视化
dev.new(width = 25, height = 5)
par(mfrow = c(1, 5), mar = c(0, 0, 0, 0))  # 设置多图布局
layout.par <- list(mass = 0.5, equil = 1, k = 0.0001, repeqdis = 0.1, kfr = 0.01, repulse = FALSE)

for (i in 1:length(networks)) {
  net <- networks[[i]]
  node_degrees <- degree(net, cmode = "freeman")
  vertex_sizes <- (node_degrees - min(node_degrees)) / (max(node_degrees) - min(node_degrees)) * 3 + 1
  layout <- gplot.layout.spring(net, layout.par = layout.par)
  # 绘制网络
  plot(
    net,
    displaylabels = FALSE,  # 隐藏节点标签
    coord = layout,
    vertex.cex = vertex_sizes,         # 节点大小
    edge.lwd = net %e% "weight" / max(net %e% "weight") * 5,  # 边宽按权重比例
    main = ""
  )
}

# 计算统计量
for (i in 1:length(networks)) {
  year <- 2014 + i
  g <- intergraph::asIgraph(networks[[i]])
  g_unweighted <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE) 
  
  # 计算统计量
  network_size <- vcount(g)
  number_of_edges <- ecount(g)
  network_density <- edge_density(g)
  #number_of_components <- components(g)$no
  number_of_isolates <- sum(igraph::degree(g) == 0)
  
  # 使用igraph包的triad.census函数
  triad_census_result <- igraph::triad.census(g_unweighted)
  
  transitivity <- transitivity(g_unweighted, type = "global")
  
  if ("weight" %in% edge_attr_names(g)) {
    sum_of_edge_weights <- sum(E(g)$weight, na.rm = TRUE)
    max_edge_weight <- max(E(g)$weight, na.rm = TRUE)
  } else {
    sum_of_edge_weights <- NA
    max_edge_weight <- NA
  }
  
  in_degree_centralization <- centr_degree(g, mode = "in", normalized = TRUE)$centralization
  
  # 打印统计量
  cat("Year:", year, "\n")
  cat("Network size:", network_size, "\n")
  cat("Number of edges:", number_of_edges, "\n")
  cat("Network density:", network_density, "\n")
  #cat("Number of components:", number_of_components, "\n")
  cat("Number of isolates:", number_of_isolates, "\n")
  cat("Sum of edge weights:", sum_of_edge_weights, "\n")
  cat("Maximum edge weight:", max_edge_weight, "\n")
  cat("In-degree centralization:", in_degree_centralization, "\n")
  cat("Triad census:", triad_census_result, "\n")
  cat("Transitivity:", transitivity, "\n")
  cat("\n")
}



# 模型显著性
load("model7.rda")
load("model11.rda")
load("model12.rda") 
load("model13.rda") 

library(ggplot2)

# 创建术语名称映射及原始顺序
term_mapping <- c(
  "edgecov.memory[[i]]" = "AUTOR",
  "absdiff.log_economic_output" = "INDUS_Ndif",
  "nodecov.log_economic_output" = "INDUS_Ncov",
  "absdiff.funded_log" = "FUNDE_Ndif",
  "nodecov.funded_log" = "FUNDE_Ncov",
  "edgecov.semantic_similarity_matrix[[i]]" = "CONTE_Ecov",
  "absdiff.group_log" = "CHILD_Ndif",
  "nodecov.group_log" = "CHILD_Ncov",
  "absdiff.alt_sqrt" = "SIBLI_Ndif",
  "nodecov.alt_sqrt" = "SIBLI_Ncov",
  "nodematch.class" = "CLASS_Nmat",
  "nodematch.section" = "SECTI_Nmat",
  "absdiff.eigenvector_sqrt" = "EIGEN_Ndif",
  "nodecov.eigenvector_sqrt" = "EIGEN_Ncov",
  "absdiff.yearly_degree_log" = "DEGRE_Ndif",
  "nodecov.yearly_degree_log" = "DEGRE_Ncov",
  "gwesp.fixed.0.25" = "GWESP",
  "edges" = "EDGES"
)

# 提取模型结果时添加因子顺序
extract_btergm_results <- function(model) {
  summary_df <- as.data.frame(summary(model))  # 转换为数据框
  summary_df$term <- rownames(summary(model))  # 添加术语列
  summary_df <- summary_df %>%
    rename(
      estimate = Estimate,
      conf.low = `2.5%`,
      conf.high = `97.5%`
    ) %>%
    mutate(
      term = term_mapping[term],
      term = factor(term, levels = term_mapping)  # 设置因子顺序
    )
  return(summary_df)
}
models <- list(
  "MODEL13" = model13,
  "MODEL12" = model12,
  "MODEL11" = model11,
  "MODEL7" = model7
)

# 整理多个模型结果
results <- do.call(rbind, lapply(names(models), function(name) {
  data <- extract_btergm_results(models[[name]])
  data$model <- name
  data
}))

# 设置模型顺序为因子并按照 models 列表顺序排列
results$model <- factor(results$model, levels = names(models))
# 绘制点图
p <- ggplot(results, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.3, size = 1.2) +
  theme_minimal(base_size = 16) +
  xlab("Coefficient Estimate") +
  ylab("") +
  scale_color_manual(values = c("#006B3C","#990033","#555555","#004C97")) + 
  guides(color = guide_legend(reverse = TRUE)) +  # 反转图例顺序
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 14),  # 坐标轴文字字号
    axis.title = element_text(size = 18, face = "bold"),  # 坐标轴标题字号
    legend.text = element_text(size = 16)  # 图例文字字号
  )



# 节点类型去分模型
p <- ggplot(results, 
            aes(x = estimate, 
                y = term, 
                shape = model,   # 将color映射改为shape映射
                group = model)) +
  geom_point(position = position_dodge(width = 0.7), 
             size = 4,         # 增大点尺寸以提高形状辨识度
             color = "black") + # 统一颜色，用形状区分模型
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7),
                 height = 0.3, 
                 linewidth = 1.2) +
  theme_minimal(base_size = 16) +
  labs(
    x = "系数估计值",          # 横轴标题中文化
    y = "", 
    shape = "模型"            # 图例标题中文化
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18),  # 定义四个模型的点形状
    labels = c("MODEL 13", "MODEL 12", "MODEL 11", "MODEL 7")
  ) +
  guides(
    shape = guide_legend(
      reverse = TRUE,          # 保持原有图例顺序设置
      override.aes = list(size = 4) # 增大图例符号尺寸
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16),  # 图例标题字号
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )


print(p)
# 保存为高清大图
output_file <- "btergm_7_13_plot.png"  # 文件名
ggsave(
  filename = output_file,
  plot = p,
  width = 12,      # 图像宽度（单位：英寸）
  height = 16,      # 图像高度（单位：英寸）
  dpi = 600        # 分辨率（DPI）
)



# 下面要画的是特征的分布变化
load("model13.rda")
future_model13 <- simulate(model13, nsim = 100)
n_nodes <- network.size(future_model13[[1]])  # 获取节点数量
edge_prob_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
for (i in seq_along(future_model13)) {
  adj_matrix <- as.matrix(future_model13[[i]])
  edge_prob_matrix <- edge_prob_matrix + adj_matrix
}
edge_prob_matrix <- edge_prob_matrix / length(future_model13)
write.csv(edge_prob_matrix,file="model13_predicted.csv")


model3_predicted <- read.csv("model3_predicted.csv")
model4_predicted <- read.csv("model4_predicted.csv")
model6_predicted <- read.csv("model6_predicted.csv")
model7_predicted <- read.csv("model7_predicted.csv")
model11_predicted <- read.csv("model11_predicted.csv")
model12_predicted <- read.csv("model12_predicted.csv")
model13_predicted <- read.csv("model13_predicted.csv")
cpc_attributes <- read.csv("cpc_node_attribute.csv")
cpc_attributes <- cpc_attributes %>% 
  dplyr::select(cpc_subclass, alt_sqrt, group_log) %>%
  dplyr::distinct(cpc_subclass, .keep_all = TRUE)

fix_prediction_matrix <- function(matrix) {
  # 确保矩阵为data.frame，避免后续操作出错
  matrix <- as.data.frame(matrix)
  # 设置行名为第一列，并移除第一列
  rownames(matrix) <- matrix[[1]]  # 第一列为行名
  matrix <- matrix[, -1]           # 删除第一列
  # 去掉列名中的“X”
  colnames(matrix) <- gsub("^X", "", colnames(matrix))
  # 确保行列一致（如有缺失，补充行或列为NA）
  all_nodes <- union(rownames(matrix), colnames(matrix))
  matrix <- matrix[all_nodes, all_nodes, drop = FALSE]
  return(as.matrix(matrix))
}

# 对四个预测矩阵依次修复
model3_predicted <- fix_prediction_matrix(model3_predicted)
model4_predicted <- fix_prediction_matrix(model4_predicted)
model6_predicted <- fix_prediction_matrix(model6_predicted)
model7_predicted <- fix_prediction_matrix(model7_predicted)
model11_predicted <- fix_prediction_matrix(model11_predicted)
model12_predicted <- fix_prediction_matrix(model12_predicted)
model13_predicted <- fix_prediction_matrix(model13_predicted)

calculate_attributes <- function(predicted_matrix, cpc_attributes) {
  edge_list <- which(predicted_matrix >= 0, arr.ind = TRUE)  # 获取所有节点对
  # 创建数据框
  edge_data <- data.frame(
    node1 = rownames(predicted_matrix)[edge_list[, 1]],
    node2 = colnames(predicted_matrix)[edge_list[, 2]],
    Prob = predicted_matrix[edge_list]  # 记录每对节点的预测概率
  )
  
  # 添加属性值
  edge_data <- edge_data %>%
    left_join(cpc_attributes, by = c("node1" = "cpc_subclass")) %>%
    rename(alt_sqrt1 = alt_sqrt, group_log1 = group_log) %>%
    left_join(cpc_attributes, by = c("node2" = "cpc_subclass")) %>%
    rename(alt_sqrt2 = alt_sqrt, group_log2 = group_log) %>%
    
    # 计算SIBLI和CHILD指标
    mutate(
      SIBLI_Ncov = alt_sqrt1 + alt_sqrt2,
      SIBLI_Ndif = abs(alt_sqrt1 - alt_sqrt2),
      CHILD_Ncov = group_log1 + group_log2,
      CHILD_Ndif = abs(group_log1 - group_log2)
    ) %>%
    
    # 根据概率将数据分为两组
    mutate(converge = ifelse(Prob > 0.5, "converge", "not converge")) %>%
    # 选择需要的列
    dplyr::select(node1, node2, SIBLI_Ncov, SIBLI_Ndif, CHILD_Ncov, CHILD_Ndif, converge)
  
  return(edge_data)
}

# 计算每个模型的连边属性值
model3_data <- calculate_attributes(model3_predicted, cpc_attributes) %>% mutate(model = "Model 3")
model4_data <- calculate_attributes(model4_predicted, cpc_attributes) %>% mutate(model = "Model 4")
model6_data <- calculate_attributes(model6_predicted, cpc_attributes) %>% mutate(model = "Model 6")
model7_data <- calculate_attributes(model7_predicted, cpc_attributes) %>% mutate(model = "Model 7")
model11_data <- calculate_attributes(model11_predicted, cpc_attributes) %>% mutate(model = "Model 11")
model12_data <- calculate_attributes(model12_predicted, cpc_attributes) %>% mutate(model = "Model 12")
model13_data <- calculate_attributes(model13_predicted, cpc_attributes) %>% mutate(model = "Model 13")

# 合并所有模型数据
all_models_data <- bind_rows(model3_data, model4_data, model6_data)
all_models_data <- bind_rows(model7_data, model11_data, model12_data, model13_data)

# 数据长格式，用于绘图
library(tidyr)
plot_data <- all_models_data %>%
  filter(node1 != node2) %>%
  pivot_longer(cols = c(SIBLI_Ncov,SIBLI_Ndif,CHILD_Ncov,CHILD_Ndif), names_to = "Metric", values_to = "Value") %>%
  mutate(model = factor(model, levels = c("Model 7", "Model 11", "Model 12", "Model 13")))

# 自定义模型颜色
model_colors <- c(
  "Model 3" = "#004C97",
  "Model 4" = "#555555",
  "Model 6" = "#990033"
)


model_colors <- c(
  "Model 7" = "#004C97",
  "Model 11" = "#555555",
  "Model 12" = "#990033",
  "Model 13" = "#006B3C"
)

# 绘制分布图
plot_boxplots <- function(data, metric_name) {
  ggplot(data %>% filter(Metric == metric_name), aes(x = converge, y = Value, fill = model)) +
    geom_boxplot(outlier.size = 1, outlier.shape = 21, alpha = 0.7) +
    scale_fill_manual(
      values = model_colors,
      name = "Model"
    ) +
    labs(
      title = paste("Distribution of", metric_name, "across Models"),
      x = "Model Prediction",
      y = metric_name
    ) +
    theme_minimal(base_size = 14) +
    theme(
      #axis.text.x = element_text(angle = 45, hjust = 1), # 旋转x轴标签
      plot.title = element_text(hjust = 0.5, face = "bold") # 标题居中加粗
    )
}

# 分别绘制四种特征的箱形图
boxplot_SIBLI_Ncov <- plot_boxplots(plot_data, "SIBLI_Ncov")
boxplot_SIBLI_Ndif <- plot_boxplots(plot_data, "SIBLI_Ndif")
boxplot_child_Ncov <- plot_boxplots(plot_data, "CHILD_Ncov")
boxplot_child_Ndif <- plot_boxplots(plot_data, "CHILD_Ndif")

print(boxplot_SIBLI_Ndif)
ggsave(filename = "SIBLI_Ncov.png",plot = boxplot_SIBLI_Ncov,
  width = 8, height = 6, dpi = 600
)
ggsave(filename = "SIBLI_Ndif.png",plot = boxplot_SIBLI_Ndif,
       width = 8, height = 6, dpi = 600
)
ggsave(filename = "CHILD_Ncov.png",plot = boxplot_child_Ncov,
       width = 8, height = 6, dpi = 600
)
ggsave(filename = "CHILD_Ndif.png",plot = boxplot_child_Ndif,
       width = 8, height = 6, dpi = 600
)

# 主干化网络的边数
install.packages("tidyverse")
library(tidyverse)


data_df <- data.frame(
  Year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
  Complete = c(17900, 19221, 21257, 24366, 27416, 29763, 31490, 32347, 35197, 35290),
  Upper25  = c(10228, 11068, 12657, 14486, 16829, 18851, 20307, 20981, 23338, 23255),
  Upper50  = c(7361, 7889, 9209, 10503, 12444, 13970, 15464, 15834, 14706, 14542),
  Upper75  = c(4052, 4416, 5200, 5377, 6416, 7339, 7494, 7803, 8110, 8086)
)


data_long <- data_df %>%
  pivot_longer(
    cols = -Year,         # 除去 Year 列，其余列都要进行转换
    names_to = "Type",    # 新列名称：网络类型
    values_to = "Count"   # 新列名称：数量
  )


plot <- ggplot(data_long, aes(x = factor(Year), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +

  scale_fill_manual(
    values = c("Complete" = "#004C97",  # 完整网络
               "Upper25"  = "#555555",  # upper=25%
               "Upper50"  = "#990033",  # upper=50%
               "Upper75"  = "#006B3C"),  # upper=75%
    labels = c("Complete", "Upper=25%", "Upper=50%", "Upper=75%")
  ) +
  labs(
    x = "Year",
    y = "Number of edges",
    fill = "network"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 16),  # 增大 x 轴标题字体
    axis.title.y = element_text(size = 16),  # 增大 y 轴标题字体
    axis.text.x = element_text(size = 14),   # 增大 x 轴刻度字体
    axis.text.y = element_text(size = 14),   # 增大 y 轴刻度字体
    legend.title = element_text(size = 14),  # 增大图例标题字体
    legend.text = element_text(size = 12),   # 增大图例文本字体
    legend.position = "top"                   # 调整图例位置（可自行修改）
  )
ggsave("network_barplot.png", plot = plot, dpi = 600, width = 10, height = 6, units = "in")
