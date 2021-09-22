# Finding the optimal weights for Hierarchical clustering:
# At least 6 clusters, highest cophenetic correlation

cluster_weight_test <- function(w_total, w_M, w_h_exp, w_cult){
  clust_w_test <- country_char_num
  
  clust_w_test$Total <- w_total * country_char_num$Total
  clust_w_test$M <- w_M * country_char_num$M
  clust_w_test$health_expenditures <- w_h_exp * country_char_num$health_expenditures
  clust_w_test$cult_Y_GE16 <- w_cult * country_char_num$cult_Y_GE16
  
  d <- dist(clust_w_test, method = "euclidean")
  
  clusters <- hclust(d, method = "ward.D2")
  
  # Ideal umber of clusters
  n_clust <- fviz_nbclust(clust_w_test, FUN = hcut, method = "silhouette")
  opt_n <- n_clust$data
  opt_n <- as.numeric(opt_n$clusters[which.max(opt_n$y)])
  
  # Cophenetic distance
  coph_dist <- cor(cophenetic(clusters), d)
  return(c(w_total, w_M, w_h_exp, w_cult, "opt_n" = opt_n, "coph_dist" = coph_dist))
}

poss_w1 <- c(1.1, 1.3, 1.5)
poss_w2 <- c(1.4, 1.6, 1.8)
w_grid <- data.frame("w_total" = poss_w1,
                     "w_M" = poss_w1,
                     "w_h_exp" = poss_w2,
                     "w_cult" = poss_w2)

w_grid <- expand.grid(w_grid)

res <- t(apply(w_grid, 1, function(x) cluster_weight_test(x[1], x[2], x[3], x[4])))
res <- res[res[,5] > 5,]
res[order(res[,6], decreasing = TRUE),][1,]
