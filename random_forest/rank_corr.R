# Computes correlation between the repeated varimp rank of the clusters and the repeated varimp rank of the countries


library(tidyverse)
library(ggpubr)
library(rstatix)



country_res_varimp <- readRDS("shinydashboard/dat/country_res_varimp.RDS")
cluster_res_varimp <- readRDS("shinydashboard/dat/cluster_res_varimp.RDS")


##Rank importance by country/cluster

#Split data by cluster groups
cluster_res_varimp_list <- split(cluster_res_varimp, cluster_res_varimp$groups)


# Order does not give same values for same importance, rank is used instead.
#create order variable (most important predictor is ranked nr. 1)  per cluster


#cluster_res_varimp_list <- lapply(cluster_res_varimp_list, function(x){
#
# x$order <-  order(x$groups, x$importance,  decreasing = TRUE)
# x 
#})

##create rank variable (most important predictor is ranked nr. 1)  per cluster
cluster_res_varimp_list <- lapply(cluster_res_varimp_list, function(x){
  
  x$rank <-  rank(-x$importance, x$groups, ties.method = 'average')
  x 
})

# Order does not give same values for same importance, rank is used instead.
#create order variable (most important predictor is ranked nr. 1)  per country
#country_res_varimp <- lapply(country_res_varimp, function(x){
#  
#  x$order <-  order(x$importance,  decreasing = TRUE)
#  x 
#})


#create rank variable (most important predictor is ranked nr. 1) per country
country_res_varimp <- lapply(country_res_varimp, function(x){
  
  x$rank <-  rank(-x$importance,  ties.method = 'average')
  x 
})


##Corr counry varimp rank vs. relevant cluster varimp rank

#Sort data by feature

country_res_varimp <- lapply(country_res_varimp, function(x){
  
  x <- x[order(x$feature),]
  x 
})

cluster_res_varimp_list <- lapply(cluster_res_varimp_list, function(x){
  
  x <- x[order(x$feature),]
  x 
})



#Corr between Cluster 1 varimp rank and its countries' varimp rank
#Austria <- cor(country_res_varimp[["Austria"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
#Belgium <-  cor(country_res_varimp[["Belgium"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
#Denmark <- cor(country_res_varimp[["Denmark"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
#Finland <- cor(country_res_varimp[["Finland"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
#Netherlands <- cor(country_res_varimp[["Netherlands"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
#Sweden <- cor(country_res_varimp[["Sweden"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))


# Which country in which cluster

clust_dat$country_code <- rownames(clust_dat)
clust_geo <- merge(clust_dat[,c("groups", "country_code")], capitals[,c("country", "country_code")], by = "country_code", all.x = TRUE)

# function to calculate correlation between varimp rank of a cluster and of all countries within
rc_cluster <- function(cluster){
  act_clust <- clust_geo[clust_geo$groups == cluster,]
  act_ct <- act_clust$country
  
  act_cor <- unlist(lapply(act_ct, function(x){
    cor(country_res_varimp[[x]][["rank"]], cluster_res_varimp_list[[cluster]][["rank"]], method = c("spearman"))
  }))
  act_res <- as.data.frame(cbind(act_ct, act_cor))
  colnames(act_res) <- c("country", "correlation")
  act_res$correlation <- as.numeric(act_res$correlation)
  return(act_res)
}

rank_corr_res <- lapply(c(1:max(clust_dat$groups)), rc_cluster)

# Save results for the visualization 'Country characteristics tab'
saveRDS(rank_corr_res, "shinydashboard/dat/rank_corr_res.RDS")



# Computes correlation between the repeated varimp rank of the clusters

  

# function to calculate correlation between varimp rank of the clusters

rc_btw_cluster <- function(cluster_btw){
  act_clust_1 <- clust_geo[clust_geo$groups == cluster_btw,]
  act_clust_2 <- clust_geo[clust_geo$groups > cluster_btw,]
  
  
  act_cor <- unlist(lapply(act_clust_2, function(x){
    cor(cluster_res_varimp_list[[x]][["rank"]], cluster_res_varimp_list[[cluster_btw]][["rank"]], method = c("spearman"))
  }))
  act_res <- as.data.frame(cbind(act_clust_2, act_cor))
  colnames(act_clust_2) <- c("cluster", "correlation")
  act_res$correlation <- as.numeric(act_res$correlation)
  return(act_res)
}

#rank_btw_corr_res <- lapply(c(1:max(clust_dat$groups)-1), rc_btw_cluster)


#Manually
cor(cluster_res_varimp_list[[1]][["rank"]], cluster_res_varimp_list[[2]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[1]][["rank"]], cluster_res_varimp_list[[3]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[1]][["rank"]], cluster_res_varimp_list[[4]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[1]][["rank"]], cluster_res_varimp_list[[5]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[1]][["rank"]], cluster_res_varimp_list[[6]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[1]][["rank"]], cluster_res_varimp_list[[7]][["rank"]], method = c("spearman"))

cor(cluster_res_varimp_list[[2]][["rank"]], cluster_res_varimp_list[[3]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[2]][["rank"]], cluster_res_varimp_list[[4]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[2]][["rank"]], cluster_res_varimp_list[[5]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[2]][["rank"]], cluster_res_varimp_list[[6]][["rank"]], method = c("spearman"))
cor(cluster_res_varimp_list[[2]][["rank"]], cluster_res_varimp_list[[7]][["rank"]], method = c("spearman"))
#etc

#Repeated measures ANOVA

#get data into required format
rma_cluster_res_varimp <- cluster_res_varimp[with(cluster_res_varimp, order(groups, feature)),]

rma_cluster_res_varimp$groups <- as.factor(rma_cluster_res_varimp$groups)
rma_cluster_res_varimp$feature <- as.factor(rma_cluster_res_varimp$feature)

#Summary
rma_cluster_res_varimp %>%
  group_by(groups) %>%
  get_summary_stats(importance, type ="mean_sd")

#ANOVA assumptions


##Normality

rma_cluster_res_varimp %>%
  group_by(groups) %>%
  shapiro_test(importance) #normality fails

ggqqplot(rma_cluster_res_varimp, "importance", facet.by = "groups") #normality fails -> non-parametric test needed

#Friedman-test on varimp between clusters

res_fried <-rma_cluster_res_varimp %>%
  friedman_test(importance ~ groups | feature )
res_fried #groups are significantly different

##Effect size
rma_cluster_res_varimp %>% friedman_effsize(importance ~ groups | feature) #small effect size :/

##Multiple pairwise-comparisons (Wilcoxon signed-rank test)
# pairwise comparisons
pwc <- rma_cluster_res_varimp %>%
  wilcox_test(importance ~ groups, paired = TRUE, p.adjust.method = "bonferroni")
pwc
print(tbl_df(pwc), n=40) #clusters significatly differ (5 pairs out of the 21 combinations): 
#                                                        1 - 4, 1- 6, 1 - 7
#                                                        4 -5, 5 -6

# Friedman test on varimp RANKINGs between clusters

# add ranks within clusters

#Unsplit cluster_res_varimp_list (contains ranks)
cluster_res_rank <- unname(cluster_res_varimp_list) #remove list names
cluster_rank <- do.call(what = "rbind", cluster_res_rank) #unsplit


res_rank_fried <-cluster_rank %>%
  friedman_test(rank ~ groups | feature )
res_rank_fried #groups are NOT significantly different






