# net construct
library(tidyverse)
library(statnet)
#library(GGally)
library(ggplot2)
#library(grid)
#library(gridExtra)
#library(Hmisc)
#library(zoo)
library(cshapes)
#library(patchwork)
library(backbone)
#library(speedglm)
#library(texreg)
library(igraph)
library(network)
library(intergraph)
#library(lme4)
#library(plm)
library(dplyr)
setwd("E:/指数随机图/patentsview")


read_and_process_matrix <- function(year) {
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  # 读取igraph对象
  g <- readRDS(file_name)
  # 转换为邻接矩阵
  matrix_adj <- as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
  # 使用 edge_weights 作为权重
  net <- as.network(
    matrix_adj,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight",  # 指定边权重的名称
    directed = FALSE
  )
  return(net)
}


# 对多个年份的数据进行处理
years <- 2005:2024
networks <- lapply(years, read_and_process_matrix)

# 转化为边列表
library(statnet)

edge_list_all_years <- data.frame(source = character(), target = character(), stringsAsFactors = FALSE)
# 存储所有出现过的节点对
all_edge_pairs <- list()

# 遍历每个年份的网络数据
for (i in 1:length(years)) {
  # 获取当前年份的网络对象
  network <- networks[[i]]
  # 获取当前年份的边列表（节点ID）
  edge_list <- as.edgelist(network)
  # 获取边的权重
  edge_weights <- network::get.edge.attribute(network, "weight")
  # 确保无向网络的边不重复，处理无向边
  edge_list <- apply(edge_list, 1, function(x) sort(x))
  edge_list <- unique(t(edge_list))  # 去除重复的边
  # 更新所有年份的节点对
  all_edge_pairs <- unique(c(all_edge_pairs, list(as.data.frame(edge_list))))
  # 使用 network.vertex.names() 来获取节点名称
  node_names <- network.vertex.names(network)
  # 生成当前年份的边列表数据框
  edge_list_year_df <- data.frame(
    source = node_names[edge_list[, 1]],
    target = node_names[edge_list[, 2]],
    weight = edge_weights
  )
  # 将当前年份的边数据框的列名修改为 "year"
  colnames(edge_list_year_df)[3] <- as.character(years[i])
  # 如果是第一次循环，直接赋值；否则使用 dplyr::left_join 来合并
  if (i == 1) {
    edge_list_all_years <- edge_list_year_df
  } else {
    # 使用by = c("source", "target")来合并，避免列名冲突
    edge_list_all_years <- left_join(edge_list_all_years, edge_list_year_df, by = c("source", "target"))
  }
}
# 更新所有年份的节点对并填充缺失的边
all_edge_pairs_df <- do.call(rbind, all_edge_pairs)
all_edge_pairs_df <- unique(all_edge_pairs_df)

# 为每对节点添加所有年份的列
for (year in years) {
  if (!(as.character(year) %in% colnames(edge_list_all_years))) {
    edge_list_all_years[as.character(year)] <- 0
  }
}
edge_list_all_years[is.na(edge_list_all_years)] <- 0
write.csv(edge_list_all_years,'edge_list_all_years.csv', row.names = FALSE)


# 添加节点和边属性
cpc_attributes <- read.csv("cpc_node_attribute.csv")
missing_attributes <- cpc_attributes %>% filter(rowSums(is.na(.)) > 0)
cpc_attributes[is.na(cpc_attributes$section), ]
#"A99Z" %in% all_nodes # false,缺失是因为计算共现网络中的统计量时，本身这个节点就不在里面
# 这里要说明的是，因为节点属性的计算中，节点度数、中心性的计算是基于共现网络的，而其他是基于cpc原表的，二者之间有差距，因此会有大量缺失值。而我们的模型其实仅关注网络中的节点，因此可以仅保留无NA的记录。
cpc_similarity3 <- read.csv("cpc_semanticsimilarity_matrix.csv") # 语义相似性
rownames(cpc_similarity3) <- cpc_similarity3$cpc_subclass  # 假设 cpc_subclass 是存储行标签的列
cpc_similarity3 <- cpc_similarity3[, -which(colnames(cpc_similarity3) == "cpc_subclass")]  # 删除 cpc_subclass 列
cpc_similarity3[cpc_similarity3 < 0] <- 0
diag(cpc_similarity3) <- 0
# 先求所有网络中节点的并集
all_nodes <- sort(unique(unlist(lapply(networks, network::network.vertex.names))))
full_sim <- cpc_similarity3[all_nodes, all_nodes, drop = FALSE]
write.csv(full_sim, file = "cpc_semanticsimilarity.csv", row.names = FALSE)

for (i in seq_along(networks)) {
  net <- networks[[i]]
  selected_year <- years[i]
  
  # 获取原网络的邻接矩阵
  old_adj <- network::as.matrix.network(net, attrname = "weight")
  # 获取原网络的节点名称
  orig_nodes <- network::network.vertex.names(net)
 
  # 2. 构造全节点邻接矩阵，行列名称均为 all_nodes
  full_adj <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes),
                     dimnames = list(all_nodes, all_nodes))
  
  # 将原网络中的边映射到全矩阵中（原网络的顶点名称应与 all_nodes 匹配）
  full_adj[orig_nodes, orig_nodes] <- old_adj
  
  # 3. 依据全节点邻接矩阵创建新网络
  new_net <- network::network(full_adj,
                              directed = network::is.directed(net),
                              matrix.type = "adjacency",
                              ignore.eval = FALSE,
                              names.eval = "weight")
  # 显式设置顶点名称，确保顺序与 all_nodes 相同
  network::set.vertex.attribute(new_net, "vertex.names", all_nodes)
 
  # 4. 节点属性赋值：构造包含 all_nodes 的数据框，再右连接当前年份的属性数据，
  #    缺失值替换为 0。因为 new_net 的顶点顺序就是 all_nodes，直接赋值即可。
  all_nodes_df <- data.frame(cpc_subclass = all_nodes,
                             year = selected_year,
                             stringsAsFactors = FALSE)
  cpc_info <- cpc_attributes %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::right_join(all_nodes_df, by = "cpc_subclass") %>%
    dplyr::arrange(match(cpc_subclass, all_nodes))
  # 处理 'section' 和 'class' 列的 NA 值
  cpc_info$section[is.na(cpc_info$section)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$section)], 1, 1)
  cpc_info$class[is.na(cpc_info$class)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$class)], 1, 3)
  # 其余用平均值代替
  cpc_info$yearly_degree_log[is.na(cpc_info$yearly_degree_log)] <- mean(cpc_info$yearly_degree_log, na.rm = TRUE)
  cpc_info$eigenvector_sqrt[is.na(cpc_info$eigenvector_sqrt)] <- mean(cpc_info$eigenvector_sqrt, na.rm = TRUE)
  cpc_info$alt_sqrt[is.na(cpc_info$alt_sqrt)] <- mean(cpc_info$alt_sqrt, na.rm = TRUE)
  cpc_info$group_log[is.na(cpc_info$group_log)] <- mean(cpc_info$group_log, na.rm = TRUE)
  cpc_info$funded_log[is.na(cpc_info$funded_log)] <- mean(cpc_info$funded_log, na.rm = TRUE)
  cpc_info$log_economic_output[is.na(cpc_info$log_economic_output)] <- mean(cpc_info$log_economic_output, na.rm = TRUE)
  # 若还有，将其设置为 0
  cpc_info[is.na(cpc_info)] <- 0
  
  new_net %v% "yearly_degree_log"      <- cpc_info$yearly_degree_log
  new_net %v% "alt_sqrt"               <- cpc_info$alt_sqrt
  new_net %v% "group_log"              <- cpc_info$group_log
  new_net %v% "eigenvector_sqrt"       <- cpc_info$eigenvector_sqrt
  new_net %v% "funded_log"             <- cpc_info$funded_log
  new_net %v% "log_economic_output"    <- cpc_info$log_economic_output
  new_net %v% "section"                <- cpc_info$section
  new_net %v% "class"                  <- cpc_info$class
  
  # 5. 边属性赋值：构造一个全节点的相似性矩阵
  full_sim <- cpc_similarity3[all_nodes, all_nodes, drop = FALSE]
  
  # 提取 new_net 中的边列表
  edge_list <- as.edgelist(new_net)
  node_names <- new_net %v% "vertex.names"
  edge_list <- data.frame(
    node1 = node_names[edge_list[, 1]],
    node2 = node_names[edge_list[, 2]]
  )
  nEdges <- network::network.edgecount(new_net)
  sim_values <- numeric(nEdges)
  if (!is.null(edge_list) && nrow(edge_list) > 0) {
    for (e in 1:nEdges) {
      tail_name <- edge_list[e, 1]
      head_name <- edge_list[e, 2]
      sim_val <- full_sim[tail_name, head_name]
      if (is.na(sim_val)) sim_val <- 0
      sim_values[e] <- sim_val
    }
  }
  new_net %e% "similarity" <- sim_values
  
  # 用新网络替换原网络
  networks[[i]] <- new_net
}

# 检查网络节点属性------------------------------------
# 获取网络中的所有节点属性名称
node_attr_names <- network::list.vertex.attributes(networks[[1]])
# 检查每个节点属性是否包含 NA
na_check <- sapply(node_attr_names, function(attr) {
  any(is.na(network::get.vertex.attribute(networks[[1]], attrname = attr)))
})
# 输出包含 NA 的属性名称
na_check[na_check == TRUE]
# -------------------------------------
saveRDS(networks, file = "networks_all.rds")
networks <- readRDS(file = "networks_all.rds")

# 第二个模型
read_and_process_matrix <- function(year) {
  # 读取网络数据
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  g <- readRDS(file_name)
  edge_weights <- E(g)$weight
  upper_threshold <- quantile(edge_weights, probs = 0.25)
  
  # 使用 global 方法进行主干化
  matrix_net <- global(
    G = g,
    upper = upper_threshold,  # 指定上限阈值
    keepzeros = FALSE,        # 去除零权重边
    class = "Matrix",         # 返回稀疏矩阵格式
    narrative = FALSE         # 显示方法介绍
  ) %>% as.matrix()
  
  # 转换为 network 对象
  net <- as.network(
    matrix_net,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight",
    directed = FALSE
  )
  return(net)
}


years <- 2005:2024
networks <- lapply(years, read_and_process_matrix)


all_nodes <- sort(unique(unlist(lapply(networks, network::network.vertex.names))))


for (i in seq_along(networks)) {
  net <- networks[[i]]
  selected_year <- years[i]
  
  # 获取原网络的邻接矩阵
  old_adj <- network::as.matrix.network(net, attrname = "weight")
  # 获取原网络的节点名称
  orig_nodes <- network::network.vertex.names(net)
  
  # 2. 构造全节点邻接矩阵，行列名称均为 all_nodes
  full_adj <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes),
                     dimnames = list(all_nodes, all_nodes))
  
  # 将原网络中的边映射到全矩阵中（原网络的顶点名称应与 all_nodes 匹配）
  full_adj[orig_nodes, orig_nodes] <- old_adj
  
  # 3. 依据全节点邻接矩阵创建新网络
  new_net <- network::network(full_adj,
                              directed = network::is.directed(net),
                              matrix.type = "adjacency",
                              ignore.eval = FALSE,
                              names.eval = "weight")
  # 显式设置顶点名称，确保顺序与 all_nodes 相同
  network::set.vertex.attribute(new_net, "vertex.names", all_nodes)
  
  # 4. 节点属性赋值：构造包含 all_nodes 的数据框，再右连接当前年份的属性数据，
  #    缺失值替换为 0。因为 new_net 的顶点顺序就是 all_nodes，直接赋值即可。
  all_nodes_df <- data.frame(cpc_subclass = all_nodes,
                             year = selected_year,
                             stringsAsFactors = FALSE)
  cpc_info <- cpc_attributes %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::right_join(all_nodes_df, by = "cpc_subclass") %>%
    dplyr::arrange(match(cpc_subclass, all_nodes))
  # 处理 'section' 和 'class' 列的 NA 值
  cpc_info$section[is.na(cpc_info$section)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$section)], 1, 1)
  cpc_info$class[is.na(cpc_info$class)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$class)], 1, 3)
  # 其余用平均值代替
  cpc_info$yearly_degree_log[is.na(cpc_info$yearly_degree_log)] <- mean(cpc_info$yearly_degree_log, na.rm = TRUE)
  cpc_info$eigenvector_sqrt[is.na(cpc_info$eigenvector_sqrt)] <- mean(cpc_info$eigenvector_sqrt, na.rm = TRUE)
  cpc_info$alt_sqrt[is.na(cpc_info$alt_sqrt)] <- mean(cpc_info$alt_sqrt, na.rm = TRUE)
  cpc_info$group_log[is.na(cpc_info$group_log)] <- mean(cpc_info$group_log, na.rm = TRUE)
  cpc_info$funded_log[is.na(cpc_info$funded_log)] <- mean(cpc_info$funded_log, na.rm = TRUE)
  cpc_info$log_economic_output[is.na(cpc_info$log_economic_output)] <- mean(cpc_info$log_economic_output, na.rm = TRUE)
  # 若还有，将其设置为 0
  cpc_info[is.na(cpc_info)] <- 0
  
  new_net %v% "yearly_degree_log"      <- cpc_info$yearly_degree_log
  new_net %v% "alt_sqrt"               <- cpc_info$alt_sqrt
  new_net %v% "group_log"              <- cpc_info$group_log
  new_net %v% "eigenvector_sqrt"       <- cpc_info$eigenvector_sqrt
  new_net %v% "funded_log"             <- cpc_info$funded_log
  new_net %v% "log_economic_output"    <- cpc_info$log_economic_output
  new_net %v% "section"                <- cpc_info$section
  new_net %v% "class"                  <- cpc_info$class
  
  # 5. 边属性赋值：构造一个全节点的相似性矩阵
  full_sim <- cpc_similarity3[all_nodes, all_nodes, drop = FALSE]
  
  # 提取 new_net 中的边列表
  edge_list <- as.edgelist(new_net)
  node_names <- new_net %v% "vertex.names"
  edge_list <- data.frame(
    node1 = node_names[edge_list[, 1]],
    node2 = node_names[edge_list[, 2]]
  )
  nEdges <- network::network.edgecount(new_net)
  sim_values <- numeric(nEdges)
  if (!is.null(edge_list) && nrow(edge_list) > 0) {
    for (e in 1:nEdges) {
      tail_name <- edge_list[e, 1]
      head_name <- edge_list[e, 2]
      sim_val <- full_sim[tail_name, head_name]
      if (is.na(sim_val)) sim_val <- 0
      sim_values[e] <- sim_val
    }
  }
  new_net %e% "similarity" <- sim_values
  
  # 用新网络替换原网络
  networks[[i]] <- new_net
}

saveRDS(networks, file = "networks_25.rds")
networks <- readRDS(file = "networks_25.rds")

# 第三个模型
read_and_process_matrix <- function(year) {
  # 读取网络数据
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  g <- readRDS(file_name)
  edge_weights <- E(g)$weight
  upper_threshold <- quantile(edge_weights, probs = 0.5)
  
  # 使用 global 方法进行主干化
  matrix_net <- global(
    G = g,
    upper = upper_threshold,  # 指定上限阈值
    keepzeros = FALSE,        # 去除零权重边
    class = "Matrix",         # 返回稀疏矩阵格式
    narrative = FALSE          # 显示方法介绍
  ) %>% as.matrix()
  
  # 转换为 network 对象
  net <- as.network(
    matrix_net,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight",
    directed = FALSE
  )
  #net %v% "weight" <- edge_weights
  return(net)
}

years <- 2005:2024
networks <- lapply(years, read_and_process_matrix)


all_nodes <- sort(unique(unlist(lapply(networks, network::network.vertex.names))))


for (i in seq_along(networks)) {
  net <- networks[[i]]
  selected_year <- years[i]
  
  # 获取原网络的邻接矩阵
  old_adj <- network::as.matrix.network(net, attrname = "weight")
  # 获取原网络的节点名称
  orig_nodes <- network::network.vertex.names(net)
  
  # 2. 构造全节点邻接矩阵，行列名称均为 all_nodes
  full_adj <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes),
                     dimnames = list(all_nodes, all_nodes))
  
  # 将原网络中的边映射到全矩阵中（原网络的顶点名称应与 all_nodes 匹配）
  full_adj[orig_nodes, orig_nodes] <- old_adj
  
  # 3. 依据全节点邻接矩阵创建新网络
  new_net <- network::network(full_adj,
                              directed = network::is.directed(net),
                              matrix.type = "adjacency",
                              ignore.eval = FALSE,
                              names.eval = "weight")
  # 显式设置顶点名称，确保顺序与 all_nodes 相同
  network::set.vertex.attribute(new_net, "vertex.names", all_nodes)
  
  # 4. 节点属性赋值：构造包含 all_nodes 的数据框，再右连接当前年份的属性数据，
  #    缺失值替换为 0。因为 new_net 的顶点顺序就是 all_nodes，直接赋值即可。
  all_nodes_df <- data.frame(cpc_subclass = all_nodes,
                             year = selected_year,
                             stringsAsFactors = FALSE)
  cpc_info <- cpc_attributes %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::right_join(all_nodes_df, by = "cpc_subclass") %>%
    dplyr::arrange(match(cpc_subclass, all_nodes))
  # 处理 'section' 和 'class' 列的 NA 值
  cpc_info$section[is.na(cpc_info$section)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$section)], 1, 1)
  cpc_info$class[is.na(cpc_info$class)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$class)], 1, 3)
  # 其余用平均值代替
  cpc_info$yearly_degree_log[is.na(cpc_info$yearly_degree_log)] <- mean(cpc_info$yearly_degree_log, na.rm = TRUE)
  cpc_info$eigenvector_sqrt[is.na(cpc_info$eigenvector_sqrt)] <- mean(cpc_info$eigenvector_sqrt, na.rm = TRUE)
  cpc_info$alt_sqrt[is.na(cpc_info$alt_sqrt)] <- mean(cpc_info$alt_sqrt, na.rm = TRUE)
  cpc_info$group_log[is.na(cpc_info$group_log)] <- mean(cpc_info$group_log, na.rm = TRUE)
  cpc_info$funded_log[is.na(cpc_info$funded_log)] <- mean(cpc_info$funded_log, na.rm = TRUE)
  cpc_info$log_economic_output[is.na(cpc_info$log_economic_output)] <- mean(cpc_info$log_economic_output, na.rm = TRUE)
  # 若还有，将其设置为 0
  cpc_info[is.na(cpc_info)] <- 0
  
  new_net %v% "yearly_degree_log"      <- cpc_info$yearly_degree_log
  new_net %v% "alt_sqrt"               <- cpc_info$alt_sqrt
  new_net %v% "group_log"              <- cpc_info$group_log
  new_net %v% "eigenvector_sqrt"       <- cpc_info$eigenvector_sqrt
  new_net %v% "funded_log"             <- cpc_info$funded_log
  new_net %v% "log_economic_output"    <- cpc_info$log_economic_output
  new_net %v% "section"                <- cpc_info$section
  new_net %v% "class"                  <- cpc_info$class
  
  # 5. 边属性赋值：构造一个全节点的相似性矩阵
  full_sim <- cpc_similarity3[all_nodes, all_nodes, drop = FALSE]
  
  # 提取 new_net 中的边列表
  edge_list <- as.edgelist(new_net)
  node_names <- new_net %v% "vertex.names"
  edge_list <- data.frame(
    node1 = node_names[edge_list[, 1]],
    node2 = node_names[edge_list[, 2]]
  )
  nEdges <- network::network.edgecount(new_net)
  sim_values <- numeric(nEdges)
  if (!is.null(edge_list) && nrow(edge_list) > 0) {
    for (e in 1:nEdges) {
      tail_name <- edge_list[e, 1]
      head_name <- edge_list[e, 2]
      sim_val <- full_sim[tail_name, head_name]
      if (is.na(sim_val)) sim_val <- 0
      sim_values[e] <- sim_val
    }
  }
  new_net %e% "similarity" <- sim_values
  
  # 用新网络替换原网络
  networks[[i]] <- new_net
}

saveRDS(networks, file = "networks_50.rds")
networks <- readRDS(file = "networks_50.rds")

# 第四个模型
read_and_process_matrix <- function(year) {
  # 读取网络数据
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  g <- readRDS(file_name)
  edge_weights <- E(g)$weight
  upper_threshold <- quantile(edge_weights, probs = 0.75)
  
  # 使用 global 方法进行主干化
  matrix_net <- global(
    G = g,
    upper = upper_threshold,  # 指定上限阈值
    keepzeros = FALSE,        # 去除零权重边
    class = "Matrix",         # 返回稀疏矩阵格式
    narrative = FALSE          # 显示方法介绍
  ) %>% as.matrix()
  
  # 转换为 network 对象
  net <- as.network(
    matrix_net,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight",
    directed = FALSE
  )
  #net %v% "weight" <- edge_weights
  return(net)
}

years <- 2005:2024
networks <- lapply(years, read_and_process_matrix)

all_nodes <- sort(unique(unlist(lapply(networks, network::network.vertex.names))))


for (i in seq_along(networks)) {
  net <- networks[[i]]
  selected_year <- years[i]
  
  # 获取原网络的邻接矩阵
  old_adj <- network::as.matrix.network(net, attrname = "weight")
  # 获取原网络的节点名称
  orig_nodes <- network::network.vertex.names(net)
  
  # 2. 构造全节点邻接矩阵，行列名称均为 all_nodes
  full_adj <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes),
                     dimnames = list(all_nodes, all_nodes))
  
  # 将原网络中的边映射到全矩阵中（原网络的顶点名称应与 all_nodes 匹配）
  full_adj[orig_nodes, orig_nodes] <- old_adj
  
  # 3. 依据全节点邻接矩阵创建新网络
  new_net <- network::network(full_adj,
                              directed = network::is.directed(net),
                              matrix.type = "adjacency",
                              ignore.eval = FALSE,
                              names.eval = "weight")
  # 显式设置顶点名称，确保顺序与 all_nodes 相同
  network::set.vertex.attribute(new_net, "vertex.names", all_nodes)
  
  # 4. 节点属性赋值：构造包含 all_nodes 的数据框，再右连接当前年份的属性数据，
  #    缺失值替换为 0。因为 new_net 的顶点顺序就是 all_nodes，直接赋值即可。
  all_nodes_df <- data.frame(cpc_subclass = all_nodes,
                             year = selected_year,
                             stringsAsFactors = FALSE)
  cpc_info <- cpc_attributes %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::right_join(all_nodes_df, by = "cpc_subclass") %>%
    dplyr::arrange(match(cpc_subclass, all_nodes))
  # 处理 'section' 和 'class' 列的 NA 值
  cpc_info$section[is.na(cpc_info$section)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$section)], 1, 1)
  cpc_info$class[is.na(cpc_info$class)] <- substr(cpc_info$cpc_subclass[is.na(cpc_info$class)], 1, 3)
  # 其余用平均值代替
  cpc_info$yearly_degree_log[is.na(cpc_info$yearly_degree_log)] <- mean(cpc_info$yearly_degree_log, na.rm = TRUE)
  cpc_info$eigenvector_sqrt[is.na(cpc_info$eigenvector_sqrt)] <- mean(cpc_info$eigenvector_sqrt, na.rm = TRUE)
  cpc_info$alt_sqrt[is.na(cpc_info$alt_sqrt)] <- mean(cpc_info$alt_sqrt, na.rm = TRUE)
  cpc_info$group_log[is.na(cpc_info$group_log)] <- mean(cpc_info$group_log, na.rm = TRUE)
  cpc_info$funded_log[is.na(cpc_info$funded_log)] <- mean(cpc_info$funded_log, na.rm = TRUE)
  cpc_info$log_economic_output[is.na(cpc_info$log_economic_output)] <- mean(cpc_info$log_economic_output, na.rm = TRUE)
  # 若还有，将其设置为 0
  cpc_info[is.na(cpc_info)] <- 0
  
  new_net %v% "yearly_degree_log"      <- cpc_info$yearly_degree_log
  new_net %v% "alt_sqrt"               <- cpc_info$alt_sqrt
  new_net %v% "group_log"              <- cpc_info$group_log
  new_net %v% "eigenvector_sqrt"       <- cpc_info$eigenvector_sqrt
  new_net %v% "funded_log"             <- cpc_info$funded_log
  new_net %v% "log_economic_output"    <- cpc_info$log_economic_output
  new_net %v% "section"                <- cpc_info$section
  new_net %v% "class"                  <- cpc_info$class
  
  # 5. 边属性赋值：构造一个全节点的相似性矩阵
  full_sim <- cpc_similarity3[all_nodes, all_nodes, drop = FALSE]
  
  # 提取 new_net 中的边列表
  edge_list <- as.edgelist(new_net)
  node_names <- new_net %v% "vertex.names"
  edge_list <- data.frame(
    node1 = node_names[edge_list[, 1]],
    node2 = node_names[edge_list[, 2]]
  )
  nEdges <- network::network.edgecount(new_net)
  sim_values <- numeric(nEdges)
  if (!is.null(edge_list) && nrow(edge_list) > 0) {
    for (e in 1:nEdges) {
      tail_name <- edge_list[e, 1]
      head_name <- edge_list[e, 2]
      sim_val <- full_sim[tail_name, head_name]
      if (is.na(sim_val)) sim_val <- 0
      sim_values[e] <- sim_val
    }
  }
  new_net %e% "similarity" <- sim_values
  
  # 用新网络替换原网络
  networks[[i]] <- new_net
}

saveRDS(networks, file = "networks_75.rds")
networks <- readRDS(file = "networks_75.rds")

# 网络边数统计
networks_all <- readRDS(file = "networks_all.rds")
networks_25 <- readRDS(file = "networks_25.rds")
networks_50 <- readRDS(file = "networks_50.rds")
networks_75 <- readRDS(file = "networks_75.rds")
net_all1120 <- networks_all[7:16]
net_251120 <- networks_25[7:16]
net_501120 <- networks_50[7:16]
net_751120 <- networks_75[7:16]
saveRDS(net_all1120, file = "networks_all_1120.rds")
saveRDS(net_251120, file = "networks_25_1120.rds")
saveRDS(net_501120, file = "networks_50_1120.rds")
saveRDS(net_751120, file = "networks_75_1120.rds")

# 绘图的时候只取2011-2020
networks_all <- readRDS(file = "networks_all_1120.rds")
networks_25 <- readRDS(file = "networks_25_1120.rds")
networks_50 <- readRDS(file = "networks_50_1120.rds")
networks_75 <- readRDS(file = "networks_75_1120.rds")


for (i in seq_along(networks_all)) {
  net <- networks_all[[i]]
  cat(network.size(net),"\n")
  cat(network.edgecount(net),"\n")
}


network_metrics <- data.frame(
  year         = integer(),
  num_nodes    = integer(),
  num_edges    = integer(),
  total_weight = numeric(),
  avg_weight   = numeric(),
  density      = numeric(),
  stringsAsFactors = FALSE
)

# 遍历网络列表
for(i in seq_along(networks_all)) {
  net <- asIgraph(networks_all[[i]])
  
  # 节点和边数
  n_nodes <- vcount(net)
  n_edges <- ecount(net)
  
  # 提取边的权重
  weights <- E(net)$weight
  if(is.null(weights)) {
    warning(paste("网络", 2010 + i, "没有权重属性！"))
    total_wt <- NA
    avg_wt   <- NA
  } else {
    total_wt <- sum(weights, na.rm = TRUE)
    avg_wt   <- if(length(weights) > 0) mean(weights, na.rm = TRUE) else NA
  }
  
  # 计算网络密度
  dens <- graph.density(net)
  
  # 将本网络的指标添加到数据框中
  network_metrics <- rbind(
    network_metrics,
    data.frame(
      year         = 2010 + i,
      num_nodes    = n_nodes,
      num_edges    = n_edges,
      total_weight = total_wt,
      avg_weight   = avg_wt,
      density      = dens,
      stringsAsFactors = FALSE
    )
  )
}

# 输出各年份网络的指标
print(network_metrics)

# --------------------------
# 计算网络相似性和权重分布相似性
similarities <- data.frame(
  year_pair = character(),
  Spearman  = numeric(),
  Jaccard   = numeric(),
  #Cosine    = numeric(),
  KL_Divergence = numeric(),
  #JS_Divergence = numeric(),
  stringsAsFactors = FALSE
)

# KL散度计算方法
kl_divergence <- function(P, Q) {

  # 将P和Q归一化为概率分布
  P_normalized <- P / sum(P)
  Q_normalized <- Q / sum(Q)
  
  # 确保P和Q没有零值（避免对数无穷大），通过设置一个很小的值
  P_normalized <- ifelse(P_normalized == 0, 1e-10, P_normalized)
  Q_normalized <- ifelse(Q_normalized == 0, 1e-10, Q_normalized)  # 用一个小的数代替零
  
  # 计算KL散度
  kl_value <- sum(P_normalized * log(P_normalized / Q_normalized))
  return(kl_value)
}

# Jensen-Shannon Divergence计算方法
js_divergence <- function(P, Q) {
  P_normalized <- P / sum(P)
  Q_normalized <- Q / sum(Q)
  P_normalized <- ifelse(P_normalized == 0, 1e-10, P_normalized)
  Q_normalized <- ifelse(Q_normalized == 0, 1e-10, Q_normalized)
  M <- 0.5 * (P + Q)
  js_value <- 0.5 * (kl_divergence(P, M) + kl_divergence(Q, M))  # 利用 KL 散度计算 JSD
  return(js_value)
}

# 逐对计算
for(i in 1:9){
  
  # 获取第 i 和第 i+1 个网络的邻接矩阵 (有权)
  A1 <- as.matrix(networks_all[[i]],   matrix.type = "adjacency", attr = "weight")
  A1 <- pmax(A1, t(A1))
  A2 <- as.matrix(networks_all[[i+1]], matrix.type = "adjacency", attr = "weight")
  A2 <- pmax(A2, t(A2))
  
  # 将邻接矩阵展开为向量
  v1 <- c(A1)
  v2 <- c(A2)

  spearman_val <- cor(v1, v2, method = "spearman")
  
  intersection_count <- sum(v1 > 0 & v2 > 0)
  union_count        <- sum(v1 > 0 | v2 > 0)
  jaccard_val        <- ifelse(union_count == 0, NA, intersection_count / union_count)
  
  #cosine_val         <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
  kl_val <- kl_divergence(v1, v2)
  #js_val <- js_divergence(v1, v2)
  
  # 将计算结果添加到数据框
  similarities <- rbind(
    similarities,
    data.frame(
      year_pair = paste0(2010 + i, "-", 2010 + (i + 1)),  # 例如 "2011-2012"
      Spearman  = spearman_val,
      Jaccard   = jaccard_val,
      KL_Divergence = kl_val,
      #JS_Divergence = js_val,
      stringsAsFactors = FALSE
    )
  )
}

# 查看计算结果
sum(similarities$KL_Divergence)/9
print(similarities)

similarities_long <- similarities %>%
  pivot_longer(
    cols      = c("Spearman", "Jaccard", "KL_Divergence"),
    names_to  = "measure",   # 新列: 指标名称
    values_to = "value"      # 新列: 指标数值
  )

# (2) 从 year_pair 中提取末尾年份，用于在 x 轴上做数值刻度
#     year_pair 类似 "2011-2012"，只取 "2012"
similarities_long <- similarities_long %>%
  mutate(year_end = as.numeric(sub(".*-", "", year_pair))) 

# (3) 用 ggplot2 画折线图
p <- ggplot(similarities_long, aes(x = year_end, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  # 设置 x 轴刻度，从 2011 到 2022
  scale_x_continuous(breaks = seq(2006, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  expand_limits(y = c(0, 1))+
  labs(
    x = "The later period", 
    y = "Similarity",
    color = "Metrics"
  ) +
  theme_minimal(base_size = 14)
print(p)


# 节点类型来区分
shape_mapping <- c("Spearman" = 16,   # 实心圆
                   "Jaccard" = 17,    # 三角形
                   "KL_Divergence" = 15)     # 方形

p <- ggplot(similarities_long, aes(x = year_end, y = value, shape = measure, group = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(2006, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  expand_limits(y = c(0, 1)) +
  labs(
    x = "后一年",
    y = "相似度",
    shape = "指标"
  ) +
  scale_shape_manual(values = shape_mapping) +
  guides(linetype = guide_legend(override.aes = list(shape = shape_mapping))) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",        # 图例放底部
        legend.key.width = unit(2, "cm"))  # 加宽线型图例

print(p)


ggsave("similarities.png", plot = p, width = 8, height = 4, dpi = 600)


# --------------------------
# 导出网络graphml
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
for (i in 1:10) {
  # net_i 是 network 包的对象
  net_i <- networks_all[[i]]
  # 将 network 对象转换为 igraph 对象
  g_i <- asIgraph(net_i)
  # 给输出文件命名
  year <- 2010 + i
  out_file <- paste0("network_", year, ".graphml")
  # 使用 igraph 包将图写出为 GraphML 格式
  write_graph(g_i, file = out_file, format = "graphml")
  cat("已输出：", out_file, "\n")
}

# --------------------------
# 构建一个基于class的同配网络
node_prefix <- substr(all_nodes, 1, 3)

adj_mat_logical <- outer(node_prefix, node_prefix, FUN = "==")

adj_mat <- 1 * adj_mat_logical

rownames(adj_mat) <- all_nodes
colnames(adj_mat) <- all_nodes
write.csv(adj_mat, file = "class_based_network.csv")

# --------------------------
# 度相关系数
library(igraph)
library(intergraph)  # 用于 network 对象转换为 igraph 对象
library(ggplot2)

years <- 2011:2020

# 原始属性名称
attributes <- c("yearly_degree_log", "eigenvector_sqrt","alt_sqrt", "group_log", 
                 "funded_log", "log_economic_output")

# 初始化存储结果的数据框
results <- data.frame()

# 遍历每个网络，计算同配性系数
for (i in seq_along(networks_all)) {
  net <- networks_all[[i]]
  g <- asIgraph(net)
  
  for (attr in attributes) {
    attr_values <- vertex_attr(g, attr)
    coeff <- assortativity(g, types1 = attr_values, directed = FALSE)
    results <- rbind(results, data.frame(year = years[i],
                                         attribute = attr,
                                         assortativity = coeff))
  }
}

# 将年份转换为数值型
results$year <- as.numeric(results$year)

# 修改属性名称为指定的新标签
results$attribute <- factor(results$attribute, 
                            levels = c("yearly_degree_log", "eigenvector_sqrt","alt_sqrt", "group_log",
                                        "funded_log", "log_economic_output"),
                            labels = c("DEGRE", "EIGEN", "SIBLI", "CHILD", "FUNDE", "INDUS"))

# 定义自定义颜色（分组且有深浅区分）：
# DEGRE 与 EIGEN：蓝色系（深蓝、浅蓝）
# SIBLI 与 CHILD：红色系（深红、浅红）
# FUNDE 与 INDUS：绿色系（深绿、浅绿）
color_mapping <- c("DEGRE" = "#08306B",   # 深蓝
                   "EIGEN" = "#4292C6",   # 浅蓝
                   "SIBLI" = "#99000d",   # 深红
                   "CHILD" = "#F08080",   # 浅红
                   "FUNDE" = "#006d2c",   # 深绿
                   "INDUS" = "#74c476")   # 浅绿

# 使用 ggplot2 绘制折线图

p <- ggplot(results, aes(x = year, y = assortativity, color = attribute, group = attribute)) +
  # 添加 y=0 虚线
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = color_mapping, name = "ATTRIBUTE") +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Assortativity") +
  theme(text = element_text(size = 12),
        # 将图例文字字号调小
        legend.text = element_text(size = 10),
        axis.text.x  = element_text(angle = 45, hjust = 1))

# 显示图形
print(p)



# 移除color_mapping颜色配置，改为线型与形状配置
install.packages("showtext")
library(showtext)
font_add("SimSun", "simsun.ttc")  # 加载系统字体
theme_update(text = element_text(family = "SimSun")) 

linetype_mapping <- c("DEGRE" = "solid", 
                      "EIGEN" = "dashed",
                      "SIBLI" = "dotted",
                      "CHILD" = "dotdash",
                      "FUNDE" = "longdash",
                      "INDUS" = "twodash")

shape_mapping <- c("DEGRE" = 16,  # 实心圆点
                   "EIGEN" = 17,  # 三角形
                   "SIBLI" = 15,  # 方形
                   "CHILD" = 18,  # 菱形
                   "FUNDE" = 8,   # 星形
                   "INDUS" = 3)   # 十字

# 修改绘图主体代码
p <- ggplot(results, aes(x = year, y = assortativity, 
                         linetype = attribute,  # 线型映射
                         shape = attribute,     # 形状映射
                         group = attribute)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +  # 线型在此映射
  geom_point(size = 2.5) +    # 节点形状在此映射
  scale_linetype_manual(values = linetype_mapping, name = "属性") +
  scale_shape_manual(values = shape_mapping, name = "属性") +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
  theme_minimal() +
  labs(x = "年份",
       y = "同配性系数") +
  theme(text = element_text(size = 12),
        # 将图例文字字号调小
        legend.text = element_text(size = 10),
        axis.text.x  = element_text(angle = 45, hjust = 1))

# 合并线型与形状图例（添加guides配置）
p + guides(linetype = guide_legend(override.aes = list(shape = shape_mapping)))

ggsave("network_assortativity.png", plot = p, width = 8, height = 6, dpi = 600)
