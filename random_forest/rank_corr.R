# Computes correlation between the repeated varimp rank of the clusters and the repeated varimp rank of the countries, 
# then creates an indicator of low/median/high corr.


#?? országos varimpnál miért kisebb a range, mint a klaszterszintûnél?
#?? miért van a klaszter predoctorok között 'groups' is, ország predictorok között pedig 'recovered_new'?
#timeslice oszthatóság még hátravan



country_res_varimp <- readRDS("shinydashboard/dat/country_res_varimp.RDS")
cluster_res_varimp <- readRDS("shinydashboard/dat/cluster_res_varimp.RDS")

cluster_res_varimp$feature

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


#Corr Austria varimp rank vs. Cluster 1 varimp rank

country_res_varimp[["Austria"]][["rank"]]

cluster_res_varimp_list[["1"]][["rank"]]

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
Austria <- cor(country_res_varimp[["Austria"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
Belgium <-  cor(country_res_varimp[["Belgium"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
Denmark <- cor(country_res_varimp[["Denmark"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
Finland <- cor(country_res_varimp[["Finland"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
Netherlands <- cor(country_res_varimp[["Netherlands"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))
Sweden <- cor(country_res_varimp[["Sweden"]][["rank"]], cluster_res_varimp_list[["1"]][["rank"]], method = c("spearman"))

> 
