# 对数据进行处理
## 将patentsview需要的表下载下来，https://patentsview.org/download/data-download-tables
setwd("E:/指数随机图/patentsview")

library(dplyr)
library(readr)
library(igraph)
library(tidyr)


# 首先将patent_id映射到CPC4位，只选择cpc_type为inventional的数据，算上出现的节点，且筛选掉在全部网络中仅出现1次的边
g_cpc_current <- read_tsv("g_cpc_current.tsv", col_names = TRUE) %>%
  filter(cpc_type == "inventional") %>%
  dplyr::select(patent_id = 1, cpc_subclass = 5)

g_cpc_current <- g_cpc_current %>%
  mutate(patent_id = as.character(patent_id))

g_patent <- read_tsv("g_patent.tsv", col_names = TRUE) %>%
  dplyr::select(patent_id = 1, patent_date = 3)

g_patent <- g_patent %>%
  mutate(patent_id = as.character(patent_id))

g_patent <- g_patent %>%
  mutate(year = substr(patent_date, 1, 4))  # 提取日期的前四个字符作为年份

merged_data <- g_cpc_current %>%
  left_join(g_patent %>% dplyr::select(patent_id, year), by = "patent_id")%>%
  filter(!is.na(year)) # 删去year为NA的记录

write_tsv(merged_data, "cpc_subclass_all.tsv")

## 一些描述性统计
cpc_subclass <- read_tsv("cpc_subclass_all.tsv") # 这是全局数据

total_cpc_subclass <- n_distinct(cpc_subclass$cpc_subclass)  # 唯一CPC4分类数量
cpc_distribution <- cpc_subclass %>%
  count(cpc_subclass, sort = TRUE)

year_distribution <- cpc_subclass %>%
  count(year, sort = TRUE)  # 统计每年申请专利数量

cat("  - 唯一CPC4分类数量:", total_cpc_subclass, "\n") # 640
cat("  - 最常见的CPC分类：\n")
print(cpc_distribution)

cat("  - 专利申请年分布：\n")
print(year_distribution,n=25)

## 共现网络

# Step 1: 构建CPC对并保留那些只有一个CPC分类号的专利
yearly_cpc_networks <- cpc_subclass %>%
  group_by(year, patent_id) %>%
  summarise(
    cpc_list = list(unique(cpc_subclass)), 
    is_single_cpc = lengths(cpc_list) == 1,  # 新增列 is_single_cpc
    .groups = "drop"
  ) %>%
  # 对于只有一个CPC的专利，将 pairs 设置为 NA；否则创建 CPC 对
  mutate(pairs = lapply(cpc_list, function(x) {
    if (length(x) > 1) {
      combn(x, 2, simplify = FALSE)
    } else {
      NA  # 对于单个CPC，设置为 NA
    } 
  })) %>%
  unnest(pairs, keep_empty = TRUE) %>%  # 展开 pairs 列，保留 NA 作为单节点
  distinct(year, patent_id, cpc_list, pairs, is_single_cpc)

saveRDS(yearly_cpc_networks, "yearly_cpc_networks.rds")
yearly_cpc_networks <- readRDS("yearly_cpc_networks.rds")

# Step 2: 计算全时段的CPC4共现次数并筛选掉共现次数为1的边
all_time_edges <- yearly_cpc_networks %>%
  filter(!is.na(pairs)) %>%  # 排除只有单个CPC的行
  mutate(pairs = lapply(pairs, function(x) sort(x))) %>%  # 将每对CPC按字典顺序排序
  unnest_wider(pairs, names_sep = "_") %>%  # 展开 pairs 列为两个列，简化后续处理
  group_by(pairs_1, pairs_2) %>%
  summarise(total_weight = n(), .groups = "drop") %>%  # 计算全时段的共现次数
  filter(total_weight > 1)  # 筛选出共现次数大于1的边

write_tsv(all_time_edges, "all_time_edges.tsv")
all_time_edges <- read_tsv("all_time_edges.tsv")

#all_time_edges <- yearly_cpc_networks %>%
  #filter(!is.na(pairs)) %>%  # 排除只有单个CPC的行
  #mutate(pairs = lapply(pairs, function(x) sort(x))) %>%  # 将每对CPC按字典顺序排序
  #unnest_wider(pairs, names_sep = "_") %>%  # 展开 pairs 列为两个列，简化后续处理
  #group_by(pairs_1, pairs_2) %>%
  #summarise(total_weight = n(), .groups = "drop")  # 计算全时段的共现次数
#cooccurrence_distribution <- all_time_edges %>%
  #count(total_weight, name = "frequency")  # 统计不同共现次数的频率
#single_occurrence_edges <- all_time_edges %>%
  #filter(total_weight == 1)  # 仅共现一次的边
#single_occurrence_count <- nrow(single_occurrence_edges)  # 仅共现一次的边的数量
#write_tsv(cooccurrence_distribution, "cooccurrence_distribution.tsv")
#print(cooccurrence_distribution)  # 输出各共现次数的分布
#print(paste("仅共现一次的边的数量:", single_occurrence_count))  # 输出仅共现一次的边的数量
#library(ggplot2)
# 绘制共现次数的分布图，x 轴为对数刻度，使用线图
#ggplot(cooccurrence_distribution, aes(x = total_weight, y = frequency)) +
  #geom_line(color = "steelblue", size = 1) +
  #geom_point(color = "steelblue", size = 1.5) +
  #scale_x_log10(name = "Co-occurrence Frequency", breaks = scales::log_breaks(base = 10)) +
  #scale_y_continuous(name = "Number of Edges", labels = scales::comma) +
  #theme_minimal(base_size = 15) +
  #theme(
    #axis.text.x = element_text(angle = 45, hjust = 1)
  #)


# Step 3: 过滤掉共现次数为1的边并按年计算每年的CPC4共现次数
edges_by_year <- yearly_cpc_networks %>%
  # 将每对 CPC 按字典顺序排序
  mutate(pairs = lapply(pairs, function(x) sort(x))) %>%
  unnest_wider(pairs, names_sep = "_") %>%
  # 仅保留在全时段中共现次数大于1的边
  semi_join(all_time_edges, by = c("pairs_1", "pairs_2")) %>%
  group_by(year, pairs_1, pairs_2) %>%
  summarise(weight = n(), .groups = "drop") %>%
  # 将 CPC 对展开为两列 node1 和 node2
  rename(node1 = pairs_1, node2 = pairs_2) %>%
  dplyr::select(year, node1, node2, weight)

write_tsv(edges_by_year, "edges_by_year.tsv")
edges_by_year <- read_tsv("edges_by_year.tsv")

# Step 4: 添加孤立节点
# 从 yearly_cpc_networks 中提取只有一个 CPC 的专利，保留为孤立节点
isolated_nodes <- yearly_cpc_networks %>%
  filter(is_single_cpc) %>%
  dplyr::select(year, node = cpc_list) %>%
  unnest(node) %>%  # 展开 cpc_list 以获取单一 CPC 节点
  distinct(year, node)

# Step 5: 创建每年的网络图，并保存到列表中
network_list_by_year <- list()

for (year in 1976:2004) {
  # 获取当前年份的边表，并去除包含 NA 的行
  edges <- edges_by_year %>%
    filter(year == !!year) %>%
    dplyr::select(node1, node2, weight) %>%
    drop_na()  # 移除包含 NA 的行
  
  # 获取当前年份的孤立节点
  isolated_nodes_for_year <- isolated_nodes %>%
    filter(year == !!year) %>%
    pull(node)
  
  # 创建图对象，添加权重
  g <- graph_from_data_frame(d = edges, directed = FALSE)
  E(g)$weight <- edges$weight
  
  # 检查孤立节点是否已经在图中，添加不在图中的节点
  isolated_nodes_to_add <- setdiff(isolated_nodes_for_year, V(g)$name)
  if (length(isolated_nodes_to_add) > 0) {
    g <- add_vertices(g, nv = length(isolated_nodes_to_add), 
                      name = isolated_nodes_to_add)
  }
  
  # 保存图对象到列表
  network_list_by_year[[as.character(year)]] <- g
  
  # 保存每年的网络到文件
  saveRDS(g, file = paste0("cpc_cooccurrence_network_", year, ".rds"))
}



# 检查网络结构

years <- 2005:2024
network_stats <- list()

for (year in years) {
  # 读入网络
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  g <- readRDS(file_name)
  
  # 简化为无权网络
  g_unweighted <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE) 
  
  # 计算统计量
  network_size <- vcount(g)
  number_of_edges <- ecount(g)
  network_density <- edge_density(g)
  number_of_components <- components(g)$no
  number_of_isolates <- sum(degree(g) == 0)
  
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
  cat("Number of components:", number_of_components, "\n")
  cat("Number of isolates:", number_of_isolates, "\n")
  cat("Sum of edge weights:", sum_of_edge_weights, "\n")
  cat("Maximum edge weight:", max_edge_weight, "\n")
  cat("In-degree centralization:", in_degree_centralization, "\n")
  cat("Triad census:", triad_census_result, "\n")
  cat("Transitivity:", transitivity, "\n")
  cat("\n")
}


# 下面是机制的计算
## 首先计算节点属性

gov_interest <- read_tsv("g_gov_interest.tsv")
cpc_gov_map <- read_tsv("cpc_subclass_all.tsv")
cpc_naics_map <- read.table("E:/2024秋季学期/指数随机图/ALP_CPC/NAICS_2007/cpc4_to_naics07_2.txt",
                            sep = ",", header = TRUE, stringsAsFactors = FALSE)
unique(cpc_naics_map$naics07_2)
# 事实上，这个映射表的cpc4和现有的cpc表有一些差异
setdiff(unique(cpc_naics_map$cpc4), unique(cpc_gov_map$cpc_subclass)) #在映射表但不在cpc表
setdiff(unique(cpc_gov_map$cpc_subclass), unique(cpc_naics_map$cpc4)) #在cpc表但不在映射表
industry_out <- read.csv("E:/2024秋季学期/指数随机图/naics/naics_output.csv", sep = ",", 
                         header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# 读入网络
library(network)
years <- 2005:2024
read_and_process_matrix <- function(year) {
  file_name <- paste0("cpc_cooccurrence_network_", year, ".rds")
  g <- readRDS(file_name)
  matrix_adj <- as_adjacency_matrix(g, sparse = FALSE)  # 转换为标准矩阵
  matrix_net <- as.matrix(matrix_adj)
  as.network(
    matrix_net,
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weight",
    directed = FALSE
  )
}
networks <- lapply(years, read_and_process_matrix)

# 1. 技术成熟度 (每年CPC节点的出现次数)
cpc_subclass <- read_tsv("cpc_subclass_all.tsv")
filtered_cpc_subclass <- cpc_subclass %>%
  filter(year >= 2005 & year <= 2024)

# 每年出现次数，并取自然对数
cpc_appearance_per_year <- filtered_cpc_subclass %>%
  group_by(cpc_subclass, year) %>%
  summarise(appearance_count = n()) %>%
  mutate(appearance_count_log = log(appearance_count+1)) %>%  # 取自然对数
  arrange(year, cpc_subclass)

# 2. 计算倾向链接效应（每年和累积的度数）
library(sna)
calculate_preferential_attachment <- function(networks) {
  all_nodes <- unique(unlist(lapply(networks, network.vertex.names)))
  num_nodes <- length(all_nodes)
  
  cumulative_degree <- rep(0, num_nodes)  # 初始化累计度数
  
  yearly_degree <- lapply(networks, function(net) {
    node_names <- network.vertex.names(net)
    deg <- sna::degree(net, gmode='graph')
    
    # 创建一个完整的度数向量，匹配全集中的节点
    deg_full <- rep(0, num_nodes)
    deg_full[match(node_names, all_nodes)] <- deg
    
    # 累加到累计度数
    cumulative_degree <<- cumulative_degree + deg_full
    list(yearly = deg_full, cumulative = cumulative_degree)
  })
  
  results <- do.call(rbind, lapply(seq_along(yearly_degree), function(i) {
    data.frame(
      node = all_nodes, 
      year = i + 2004,  # 假设第一年是2005年
      yearly_degree = yearly_degree[[i]]$yearly,
      cumulative_degree = yearly_degree[[i]]$cumulative
    )
  }))
  results <- results %>% replace_na(list(yearly_degree = 0, cumulative_degree = 0))
  # 将度数取自然对数
  results <- results %>%
    mutate(
      yearly_degree_log = ifelse(yearly_degree > 0, log(yearly_degree+1),0),
      cumulative_degree_log = ifelse(cumulative_degree > 0, log(cumulative_degree+1),0)
    )
  
  return(results)
}

cpc_degree <- calculate_preferential_attachment(networks)

# cpc树的一些计算
cpc_tree <- read_tsv("g_cpc_current.tsv", col_names = TRUE) %>%
  dplyr::select(cpc_section = 3, cpc_class = 4, cpc_subclass = 5, cpc_group = 6) %>%
  distinct()  # 去除重复行

# 计算每个 cpc_subclass 对应的三位码下的其他四位码数量（替代技术数量）
cpc_subclass_alternatives <- cpc_tree %>%
  group_by(cpc_class) %>%
  mutate(alt_count = n_distinct(cpc_subclass) - 1) %>%  # 计算每个cpc_class下的替代subclass数量
  ungroup() %>%
  mutate(
    alt_log = ifelse(alt_count > 0, log(alt_count+1), 0)
  ) %>%
  dplyr::select(cpc_subclass, alt_count, alt_log) %>%
  distinct()

# 计算每个 cpc_subclass 对应的 cpc_group 数量
cpc_group_counts <- cpc_tree %>%
  group_by(cpc_subclass) %>%
  summarise(group_count = n_distinct(cpc_group)) %>%
  mutate(
    group_log = ifelse(group_count > 0, log(group_count+1), 0)
  )

# 计算中心性指标
calculate_centrality <- function(networks) {
  all_nodes <- unique(unlist(lapply(networks, network.vertex.names)))
  num_nodes <- length(all_nodes)
  
  centrality_results <- do.call(rbind, lapply(seq_along(networks), function(i) {
    net <- networks[[i]]
    node_names <- network.vertex.names(net)
    
    # 计算四类中心性
    deg <- sna::degree(net)
    clos <- sna::closeness(net)
    bet <- sna::betweenness(net)
    eig <- sna::evcent(net)
    
    # 创建一个完整的向量，匹配全集中的节点
    deg_full <- rep(NA, num_nodes)
    clos_full <- rep(NA, num_nodes)
    bet_full <- rep(NA, num_nodes)
    eig_full <- rep(NA, num_nodes)
    
    deg_full[match(node_names, all_nodes)] <- deg
    clos_full[match(node_names, all_nodes)] <- clos
    bet_full[match(node_names, all_nodes)] <- bet
    eig_full[match(node_names, all_nodes)] <- eig
    
    data.frame(
      node = all_nodes, 
      year = i + 2004, 
      degree = deg_full,
      closeness = clos_full,
      betweenness = bet_full,
      eigenvector = eig_full
    )
  }))
  centrality_results <- centrality_results %>% 
    replace_na(list(
      degree = 0,
      betweenness = 0,         # 替换介数中心性的 NA 为 0
      closeness = 0,           # 替换接近度中心性的 NA 为 0
      eigenvector = 0          # 替换特征向量中心性的 NA 为 0
    ))
  return(centrality_results)
}

centrality_data <- calculate_centrality(networks)

centrality_data <- centrality_data %>%
  mutate(
    degree_log = ifelse(degree > 0, log(degree+1), 0),
    closeness_log = ifelse(closeness > 0, log(closeness+1), 0),
    betweenness_log = ifelse(betweenness > 0, log(betweenness+1), 0),
    eigenvector_log = ifelse(eigenvector > 0, log(eigenvector+1), 0)
  )

# 3. 技术资助投入 (政府资助)
cpc_gov_map <- cpc_gov_map %>% distinct(cpc_subclass, patent_id, .keep_all = TRUE)  # 先去重

calculate_gov_funding <- function(cpc_gov_map, gov_interest) {
  # 过滤出受政府资助的专利对应的cpc_subclass
  gov_funded_cpcs <- cpc_gov_map %>%
    filter(patent_id %in% gov_interest$patent_id) %>%
    group_by(cpc_subclass) %>%
    summarise(funded_count = n())  # 计算每个cpc_subclass中受政府资助的专利数
  
  # 将结果合并到原始的cpc_gov_map数据框中
  result <- cpc_gov_map %>%
    left_join(gov_funded_cpcs, by = "cpc_subclass") %>%
    mutate(funded_count = ifelse(is.na(funded_count), 0, funded_count)) %>%
    mutate(funded_log = ifelse(funded_count > 0, log(funded_count+1), 0)) %>%
    dplyr::select(cpc_subclass, funded_count, funded_log) %>%
    distinct()
  
  return(result)
}

cpc_fund <- calculate_gov_funding(cpc_gov_map, gov_interest)

#funded_count <- sum(cpc_fund$funded == 1)    # 收到资助的数量
#not_funded_count <- sum(cpc_fund$funded == 0) # 没有收到资助的数量

#cat("收到资助的CPC数量:", funded_count, "\n") # 16565799
#cat("没有收到资助的CPC数量:", not_funded_count, "\n") # 32005
# 这个结果不平衡。资助专利映射到CPC是否是合适的？
write.csv(cpc_fund, "cpc_funding.csv", row.names = FALSE)

# 4. NAICS 映射
# 展开年份列，将数据转换成长格式，保持 description 列正确关联
industry_long <- industry_out %>%
  gather(key = "year", value = "output", `2005`:`2024`)%>%
  mutate(output = as.numeric(gsub(",", "", as.character(output))))

# 使用case_when和mutate来拆分naics
expanded_out <- industry_long %>%
  mutate(naics = case_when(
    naics == "31-33" ~ "31,32,33",   # 处理31-33
    naics == "44-45" ~ "44,45",       # 处理44-45
    naics == "48-49" ~ "48,49",       # 处理48-49
    TRUE ~ naics                      # 保持其他代码不变
  )) %>%
  separate_rows(naics, sep = ",")

# 筛选2015-2019年的数据
filtered_out <- expanded_out %>%
  filter(year >= 2005 & year <= 2024)

# 将映射关系按probability_weight计算
weighted_map <- cpc_naics_map %>%
  group_by(cpc4) %>%
  summarise(
    weighted_naics = list(naics07_2),  # 将naics07_2作为列表
    weight = list(probability_weight)     # 将probability_weight作为列表
  ) %>%
  unnest(c(weighted_naics, weight))      # 展开这两个列表

# 将 weighted_naics 转换为字符型
weighted_map <- weighted_map %>%
  mutate(weighted_naics = as.character(weighted_naics))


# 创建一个空的 data.frame 来存储最终的结果
cpc_economic_data <- data.frame(cpc4 = character(), year = integer(), log_economic_output = numeric())
years <- 2005:2024
for (year in years) {
  yearly_out <- filtered_out %>%
    filter(year == !!year)
  
  # 将每个 CPC4 映射到经济数据上，并计算年度经济产出
  cpc_econ_matrix <- weighted_map %>%
    inner_join(yearly_out, by = c("weighted_naics" = "naics")) %>%
    group_by(cpc4) %>%
    summarise(economic_output = sum(output * weight, na.rm = TRUE), .groups = "drop")
  
  # 计算自然对数，处理 economic_output 为 0 的情况
  cpc_long <- cpc_econ_matrix %>%
    mutate(year = year,  # 添加年份列
           log_economic_output = ifelse(economic_output > 0, log(economic_output), NA))
  
  # 将每一年的结果追加到最终数据框中
  cpc_economic_data <- bind_rows(cpc_economic_data, cpc_long)
}

# 只保留 cpc4、year 和 log_economic_output 列
cpc_economic_data <- cpc_economic_data %>%
  dplyr::select(cpc4, year, log_economic_output)


# 合并所有数据
merged_data <- cpc_appearance_per_year %>%
  left_join(cpc_subclass_alternatives, by = "cpc_subclass") %>%
  left_join(cpc_group_counts, by = "cpc_subclass") %>%
  left_join(cpc_degree, by = c("cpc_subclass" = "node", "year")) %>%
  left_join(centrality_data, by = c("cpc_subclass" = "node", "year")) %>%
  left_join(cpc_fund, by = "cpc_subclass") %>%
  left_join(cpc_economic_data, by = c("cpc_subclass" = "cpc4", "year")) %>%
  mutate(section = substr(cpc_subclass, 1, 1)) %>%  # 截取cpc_subclass第一位字符
  mutate(class = substr(cpc_subclass, 1, 3))# 截取cpc_subclass前三位位字符

merged_data %>% filter(is.na(yearly_degree))

closeness_centrality_sqrt <- sqrt(merged_data$closeness)
merged_data$closeness_sqrt <- closeness_centrality_sqrt

eigenvector_centrality_sqrt <- sqrt(merged_data$eigenvector)
merged_data$eigenvector_sqrt <- eigenvector_centrality_sqrt

alt_sqrt <- sqrt(merged_data$alt_count)
merged_data$alt_sqrt <- alt_sqrt
merged_data[is.na(merged_data$log_economic_output), 1:2]
# 输出合并后的结果到CSV
write.csv(merged_data, "cpc_node_attribute.csv", row.names = FALSE)
summary(merged_data$yearly_degree_log)

#calculate_naics_mapping <- function(cpc_naics_map) {
  ## 获取每个CPC对应的所有NAICS映射
  #naics_mapping <- cpc_naics_map %>%
  #  group_by(cpc4) %>%
  #  mutate(naics07_6 = as.character(naics07_1)) %>%
  #  distinct(cpc4, naics07_1) %>%  # 保证每对CPC和NAICS组合唯一
  #  ungroup()
  
  ## 创建one-hot编码
  #naics_one_hot <- naics_mapping %>%
  #  pivot_wider(names_from = naics07_1, values_from = naics07_1, values_fn = length, values_fill = 0) %>%
  #  mutate(across(-c(cpc4), ~ ifelse(. > 0, 1, 0)))  # 将非零值替换为1（one-hot）
  
 #return(naics_one_hot)
#}

#naics_mapping_one_hot <- calculate_naics_mapping(cpc_naics_map)

#write.csv(naics_mapping_one_hot, file = "cpc_naics_mapping.csv", row.names = FALSE)

# 下面计算技术领域相似性
g_cpc_current <- read_tsv("g_cpc_current.tsv", col_names = TRUE) %>%
  filter(cpc_type == "inventional") %>%
  dplyr::select(cpc_section = 3, cpc_class = 4, cpc_subclass = 5)
# 构建一个cpc_subclass的图，节点是cpc_subclass，边是层级关系
build_cpc_graph <- function(data) {
  # 构建从section到class的边
  section_class_edges <- data %>%
    distinct(cpc_section, cpc_class) %>%
    transmute(from = cpc_section, to = cpc_class)
  
  # 构建从class到subclass的边
  class_subclass_edges <- data %>%
    distinct(cpc_class, cpc_subclass) %>%
    transmute(from = cpc_class, to = cpc_subclass)
  
  # 将所有的边连接在一起
  edges <- bind_rows(section_class_edges, class_subclass_edges)
  
  # 构建有向图
  g <- graph_from_data_frame(edges, directed = TRUE)
  return(g)
}

# 计算最短路径l
calculate_path_metrics <- function(g, subclass_nodes) {
  subclass_pairs <- combn(subclass_nodes, 2, simplify = TRUE)
  
  similarity_matrix <- matrix(0, nrow = length(subclass_nodes), ncol = length(subclass_nodes))
  rownames(similarity_matrix) <- subclass_nodes
  colnames(similarity_matrix) <- subclass_nodes
  
  for (i in 1:ncol(subclass_pairs)) {
    cpc1 <- subclass_pairs[1, i]
    cpc2 <- subclass_pairs[2, i]
    
    # 1. 计算最短路径长度 l
    l <- distances(g, v = cpc1, to = cpc2, mode = "all") # 忽略方向
    if (is.infinite(l)) {
      l <- 9999  # 或者其他适合的较大值，表示远离
    }
    l <- 1/l
    # 将结果存入矩阵
    similarity_matrix[cpc1, cpc2] <- l
    similarity_matrix[cpc2, cpc1] <- l  # 对称
  }
  
  return(similarity_matrix)
}


# 计算共同父节点深度d
calculate_depth_metrics <- function(g, subclass_nodes) {
  subclass_pairs <- combn(subclass_nodes, 2, simplify = TRUE)
  
  similarity_matrix <- matrix(0, nrow = length(subclass_nodes), ncol = length(subclass_nodes))
  rownames(similarity_matrix) <- subclass_nodes
  colnames(similarity_matrix) <- subclass_nodes
  
  for (i in 1:ncol(subclass_pairs)) {
    cpc1 <- subclass_pairs[1, i]
    cpc2 <- subclass_pairs[2, i]
    # 2. 查找最近共同父节点并计算深度 d
    # 找到所有根节点（即没有入边的节点）
    find_roots <- function(g) {
      roots <- V(g)[degree(g, mode = "in") == 0]$name
      return(roots)
    }

    # 计算节点到根节点的深度
    calculate_depth <- function(g, node) {
      # 找到所有根节点
      roots <- find_roots(g)
      
      # 计算该节点的深度，找到离它最近的根节点
      min_dist <- Inf
      for (root in roots) {
        dist <- distances(g, v = root, to = node)
        if (dist < min_dist) {
          min_dist <- dist
        }
      }
  
      # 如果找到了对应的树，返回深度值
      if (min_dist < Inf) {
        return(min_dist + 1)  # 根节点深度设为1
      } else {
        return(NA)  # 如果没有找到任何路径，返回NA
      }
    }

    # 计算两个节点的最近共同父节点的深度
    common_ancestor_depth <- function(g, cpc1, cpc2) {
      ancestors1 <- subcomponent(g, cpc1, mode = "in")
      ancestors2 <- subcomponent(g, cpc2, mode = "in")
      
      # 交集得到最近的共同父节点
      common_ancestors <- intersect(ancestors1, ancestors2)
      
      if (length(common_ancestors) > 0) {
        # 对每个共同祖先计算深度，并返回最深的那个
        depth_d <- max(sapply(common_ancestors, function(ancestor) calculate_depth(g, ancestor)))
      } else {
        if (cpc1 == cpc2) {
          depth_d <- 3
        }
        depth_d <- 0  # 没有共同祖先时深度为0
      }
      
      return(depth_d)
    }
    depth_d <- common_ancestor_depth(g, cpc1, cpc2)

    # 将结果存入矩阵
    similarity_matrix[cpc1, cpc2] <- depth_d
    similarity_matrix[cpc2, cpc1] <- depth_d  # 对称
  }

  return(similarity_matrix)
}


# 计算 AA 指标
calculate_AA_metrics <- function(g, subclass_nodes) {
  # 计算节点对的组合
  subclass_pairs <- combn(subclass_nodes, 2, simplify = TRUE)
  
  # 初始化相似度矩阵
  similarity_matrix <- matrix(0, nrow = length(subclass_nodes), ncol = length(subclass_nodes))
  rownames(similarity_matrix) <- subclass_nodes
  colnames(similarity_matrix) <- subclass_nodes
  
  for (i in 1:ncol(subclass_pairs)) {
    cpc1 <- subclass_pairs[1, i]
    cpc2 <- subclass_pairs[2, i]
    
    # 1. 计算共同邻居
    common_neighbors <- intersect(neighbors(g, cpc1), neighbors(g, cpc2))
    
    # 2. 计算 AA 指标
    aa_score <- 0
    for (w in common_neighbors) {
      degree_w <- degree(g, v = w)
      aa_score <- aa_score + 1 / log(degree_w)
    }
    
    # 将结果存入矩阵
    similarity_matrix[cpc1, cpc2] <- aa_score
    similarity_matrix[cpc2, cpc1] <- aa_score  # 对称
  }
  
  return(similarity_matrix)
}

# 主函数，生成相似性矩阵
generate_similarity_matrix <- function(g_cpc_current) {
  # 构建层次结构图
  g <- build_cpc_graph(g_cpc_current)
  
  # 获取所有的 cpc_subclass 节点
  subclass_nodes <- g_cpc_current$cpc_subclass
  
  # 移除 NA 值
  subclass_nodes <- na.omit(subclass_nodes)
  
  # 确保这些 subclass 节点在图 g 中存在
  subclass_nodes <- unique(subclass_nodes[subclass_nodes %in% V(g)$name])
  
  # 计算 cpc_subclass 节点对的相似性矩阵
  similarity_matrix1 <- calculate_path_metrics(g, subclass_nodes)
  similarity_matrix2 <- calculate_depth_metrics(g, subclass_nodes)
  #similarity_matrix3 <- calculate_AA_metrics(g, subclass_nodes)
  
  return(list(similarity_matrix1 = similarity_matrix1
              #, similarity_matrix2 = similarity_matrix2
              #, similarity_matrix3 = similarity_matrix3
              ))
}

# 读取g_cpc_current并生成相似性矩阵
similarity_matrix1 <- generate_similarity_matrix(g_cpc_current)$similarity_matrix1
#similarity_matrix2 <- generate_similarity_matrix(g_cpc_current)$similarity_matrix2
#similarity_matrix3 <- generate_similarity_matrix(g_cpc_current)$similarity_matrix3
diag(similarity_matrix1) <- 1
# 保存相似性矩阵为csv文件
write.csv(similarity_matrix1, "cpc_subclass_path_matrix.csv")
#write.csv(similarity_matrix2, "cpc_subclass_depth_matrix.csv")
#write.csv(similarity_matrix3, "cpc_subclass_AA_matrix.csv")

# 语义距离计算，在python中使用all_MiniLM_L6_v2模型计算
semanticsimilarity <- read_csv("cpc_semanticsimilarity_matrix.csv", col_names = TRUE)
