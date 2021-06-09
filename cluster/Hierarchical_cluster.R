# Assigning countries to clusters with hierarchical clustering

library(stats)
library(factoextra)
library(tidyverse)
library(cowplot)
library(maps)

# Remove countries with no fb data
c_remove <- c("CY", "EE", "LV", "LT", "LU", "MT")
country_char_cl <- country_char[-which(country_char$geo %in% c_remove),]

## Selecting variables for clustering
# Merging groups
country_char_cl$Y_LT20 <- country_char_cl$`Y_LT5` + country_char_cl$`Y5-9` + country_char_cl$`Y10-14` + country_char_cl$`Y15-19`
country_char_cl$`Y20-39` <- country_char_cl$`Y20-24` + country_char_cl$`Y25-29` + country_char_cl$`Y30-34` + country_char_cl$`Y35-39`
country_char_cl$`Y40-59` <- country_char_cl$`Y40-44` + country_char_cl$`Y45-49` + country_char_cl$`Y50-54` + country_char_cl$`Y55-59`
country_char_cl$`Y60-79` <- country_char_cl$`Y60-64` + country_char_cl$`Y65-69` + country_char_cl$`Y70-74` + country_char_cl$`Y75-79`

clust_vars <- c("Total", "M", "health_expenditures",
                "cult_Y_GE16", 
                "Y_LT20", "Y20-39", "Y40-59", "Y60-79", "Y_GE80")

clust_dat <- as.data.frame(country_char_cl[,clust_vars])

# Size of population in subgroups males, age groups proportionate to population size (cult participation is already %)
clust_dat[, which(colnames(clust_dat) %in% c("M", "Y_LT20", "Y20-39", "Y40-59", "Y60-79", "Y_GE80"))] <-
  100*clust_dat[, which(colnames(clust_dat) %in% c("M", "Y_LT20", "Y20-39", "Y40-59", "Y60-79", "Y_GE80"))]/clust_dat$Total

# Health expenditures: 1000 Euro per capita
clust_dat$health_expenditures <- 1000*clust_dat$health_expenditures/clust_dat$Total

# Scaling variables
country_char_num <- as.data.frame(scale(clust_dat))

# Bigger weight for population size, percentage of males, health expenditures and cultural participation

# 7
country_char_num$Total <- 1.1 * country_char_num$Total
country_char_num$M <- 1.1 * country_char_num$M
country_char_num$health_expenditures <- 1.6 * country_char_num$health_expenditures
country_char_num$cult_Y_GE16 <- 1.6 * country_char_num$cult_Y_GE16

# Hierarchical clustering
row.names(country_char_num) <- country_char_cl$geo

# Distance object
d <- dist(country_char_num, method = "euclidean")

clusters <- hclust(d, method = "ward.D2")

fviz_dend(clusters, cex = .8, k = 1, k_colors = "black", lwd = 1.3, main = "Dendogram")

# Ideal umber of clusters
n_clust <- fviz_nbclust(country_char_num, FUN = hcut, method = "silhouette")
n_clust
opt_n <- n_clust$data
opt_n <- as.numeric(opt_n$clusters[which.max(opt_n$y)])
opt_n

# Cut the tree to 7 groups
groups <- cutree(clusters, k = 7)
table(groups)

plot(clusters, cex = 0.6)
rect.hclust(clusters, k = 7, border = 2:7)

# Cophenetic distance
cor(cophenetic(clusters), d)

clust_dat <- cbind(clust_dat, groups)

# cluster summaries
clust_res <- as.data.frame(clust_dat) %>%
  group_by(groups) %>%
  summarise_all(mean)
# Adding cluster members
clust_res$countries <- NA
for(i in 1:nrow(clust_res))
{
  clust_res$countries[i] <- paste(names(groups[which(groups == i)]), collapse = ", ")
}
res_to_write <- apply(clust_res, 2, as.character)
res_to_write <- apply(res_to_write, 2, function(x) str_replace(x, "\\.", ","))
write.csv2(res_to_write, "clusters.csv")

## Visualization of results

# Rename variables
colnames(clust_dat) <- c("Population size", "Males", "Health Expenditures", "Cultural participation",
                         "Under 20", "20-39", "40-59", "60-79", "Above 80", "groups")

# Plot of variables Total, M, Healthcare exp and cult part in a table
gr_vars <- c("Population size", "Males", "Health Expenditures", "Cultural participation")

# ALl combinations of the variables
plot_comb <- do.call(rbind, combn(gr_vars, 2, simplify = FALSE))
plot_comb <- plot_comb[nrow(plot_comb):1,]

# Creating the table
plot_table <- as.data.frame(matrix(ncol = length(unique(plot_comb[,1])), nrow = length(unique(plot_comb[,2]))))
rownames(plot_table) <- unique(plot_comb[,1])
colnames(plot_table) <- unique(plot_comb[,2])

# Fillind the lower triangle of the table
plot_names <- apply(plot_comb, 1 , paste , collapse = "*" )
plot_names2 <- plot_names
for(i in 1: nrow(plot_table)){
  for(j in 1:ncol(plot_table)){
    if(i >= j){
      plot_table[i, j] <- plot_names2[1]
      plot_names2 <- plot_names2[-1]
    }
  }
}

plot_table <- cbind(rownames(plot_table), plot_table)
plot_table <- rbind(colnames(plot_table), plot_table)
plot_names <- as.vector(t(plot_table))
plot_names[which(grepl("rownames", plot_names))] <- NA

# Creating the plots
plot_list <- list()
plot_list <- lapply(plot_names, function(x){
  if(grepl("\\*", x)){
    act_names <- strsplit(x, "\\*")[[1]]
    rown <- act_names[1]
    coln <- act_names[2]
    fviz_cluster(list(data = clust_dat, cluster = groups), choose.vars = c(coln, rown), main = "",
                 xlab = "", ylab = "") +
      scale_fill_discrete(guide = FALSE) +
      theme(legend.position = "none")
  }else if(!(is.na(x))){
    ggplot() + 
      annotate("text", x = 0, y = 0, size = 5, label = x) + 
      theme_void()
  }else{
    NULL
  }
})
names(plot_list) <- plot_names

# Replace last plot with one with legend
last_name <- strsplit(plot_names[length(plot_names)], "\\*")[[1]]
rown <- last_name[1]
coln <- last_name[2]
last_plot <- fviz_cluster(list(data = clust_dat, cluster = groups), choose.vars = c(coln, rown), main = "", xlab = "", ylab = "")
plot_list[[length(plot_list)]] <- last_plot

cluster_grid <- plot_grid(plotlist = plot_list, ncol = ncol(plot_table), rel_heights = c(0.8, rep(3, (ncol(plot_table)-1))),
          rel_widths = c(1.5, rep(3, (nrow(plot_table)-2)), 3.5))
ggsave("clusters.png", cluster_grid, width = 15, height = 12)

#Age groups
colnames(clust_res) <- c("cluster", colnames(clust_dat)[-ncol(clust_dat)], "countries")
agegr <- clust_res[, c("cluster", "Under 20", "20-39", "40-59", "60-79", "Above 80")]
agegrl <- reshape2::melt(agegr, id.vars = "cluster")
agegrl$cluster <- as.character(agegrl$cluster)

age_groups <- ggplot(agegrl, aes(x = variable, y = value, group = cluster)) +
  geom_line(aes(color = cluster), lwd = 1) +
  geom_point(aes(color = cluster), lwd = 3) +
  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38", "#38CBCE", "#68A0FE", "#F564E3", "#b87ba9")) +
  ggtitle("Age groups") +
  theme_minimal()
ggsave("age_groups.png", age_groups, width = 10, height = 6)

# Visualization of clusters on a map
# Prepare data with clusters
map_cluster <- cbind(clust_dat, "country_code" = rownames(clust_dat))
map_cluster <- merge(map_cluster, capitals[, c("country", "country_code_iso2")], by.x = "country_code", by.y = "country_code_iso2")
map_cluster <- map_cluster %>%
  dplyr::select(country, groups) %>%
  dplyr::rename(region = country, cluster = groups)
map_cluster$cluster <- as.factor(map_cluster$cluster)
# Replace "Czechia" with "Czech Republic" to match with the maps
map_cluster[which(map_cluster$region == "Czechia"), "region"] <- "Czech Republic"

# Selecting countries to visualize
map_countries <- c(unique(capitals$country), "Czech Republic")
map <- map_data("world")
eu_map <- map_data("world", region = map_countries)
saveRDS(eu_map, "shinydashboard/dat/eu_map.RDS")
eu_clusters_map <- left_join(map_cluster, eu_map, by = "region")
saveRDS(eu_clusters_map, "shinydashboard/dat/eu_clusters_map.RDS")

region_lab <- eu_clusters_map %>%
  group_by(region) %>%
  dplyr::summarise(long = mean(long), lat = mean(lat))
saveRDS(region_lab, "shinydashboard/dat/region_lab.RDS")

ggplot(eu_clusters_map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster), color = "white") +
  geom_text(aes(label = region), data = region_lab,  size = 3, hjust = 0.5) +
  scale_fill_manual(values=c("#F8766D", "#B79F00", "#00BA38", "#38CBCE", "#68A0FE", "#F564E3", "#b87ba9")) +
  coord_fixed() +
  theme_void()

# Saving results
saveRDS(clust_dat, 'shinydashboard/dat/clust_dat.RDS')
