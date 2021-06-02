# Computes correlation between the repeated varimp rank of the clusters and the repeated varimp rank of the countries, 
# then creates an indicator of low/median/high corr.

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
saveRDS(rank_corr_res, "shinydashboard/dat/rank_corr_res.RDS")
