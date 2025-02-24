#0. Installing and Importing Packages ----

install.packages("tidyverse") #run this is you needed to install R again
install.packages("fastDummies") #for one hot encoding
install.packages("cluster") #clustering and cluster visualization
install.packages("factoextra") #for visualization
install.packages("plotly") #interactive plot
install.packages("pheatmap") #heat-map
install.packages('igraph') #for fviz_dent
install.packages('mclust') #for statistical model
install.packages("dbscan") #dbscan

library("ggplot2")
library("dplyr") #for data manipulation
library("fastDummies")
library("cluster")
library("factoextra") # to use elbow method to see how many clusters
library("plotly")
library("pheatmap")
library("igraph")
library("mclust")
library("dbscan")

#1. Loading and Pre-Processing Data ---- ########################################
#1.1 Loading Data
main_data<-read.csv("adult_data_test.csv", header = TRUE)
View(main_data)

#1.2 Removing unnecessary columns
# using the -ve sign with the vector to remove selected columns
# faster as there are a smaller no. of columns to remove
main_data <- main_data[, -c(3, 5)]
View(main_data)

#1.3 Changing " ?" values to NA and removing them
total_empty_cells <- sum(main_data == " ?")
total_empty_cells

main_data[main_data==" ?"] <- NA

main_data <- na.omit(main_data)

#1.4 Converting Categorical Data into Numeric Data (One Hot Encoding)

# separating numerical data from character data
numeric_data <- main_data %>% select_if(is.numeric) 
View(numeric_data)

categorical_data <- main_data %>% select_if(is.character) 
View(categorical_data)

# one-hot-encoding: Create one column for each category in each column if not binary
?dummy_cols
one_hot_encoded <- dummy_cols(categorical_data, remove_first_dummy = TRUE)
View(one_hot_encoded)
one_hot_encoded<-one_hot_encoded[,10:100]
View(one_hot_encoded)

# creating the main data set
final_data <- cbind(numeric_data,one_hot_encoded)
View(final_data)

# renaming misspelled column 
names(final_data)
names(final_data)[2] <- "capital_gain"
View(final_data)

#1.5 Standardizing Numerical Features: Used when features have different 
# units or variances and need to contribute equally to the analysis.
numeric_cols <- c("age", "capital_gain", "capital_loss", "hours_per_week")
final_data[numeric_cols] <- scale(final_data[numeric_cols])
View(final_data)

#2. Identifying columns to use ---- ########################################
colnames(final_data)

#2.1 Correlation analysis
correlation_matrix <- cor(final_data[numeric_cols])
print(correlation_matrix)

# Visualizing the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "circle")

#2.2 Principal component analysis (PCA)
library(FactoMineR)
library(factoextra)

# Performing PCA on the final dataset
pca_result <- PCA(final_data, scale.unit = TRUE, ncp = 10, graph = FALSE)

# Scree plot - helps visualize the variance explained by each principal component
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualize - shows contribution of variables to the first two principal components
fviz_pca_var(pca_result, col.var = "contrib")


#2.3 Creating subset of data
colnames(final_data)

selected_columns <- c(numeric_cols,
                      "country_ United-States",
                      "race_ White",
                      "marital_status_ Married-civ-spouse",
                      "income_ >50K")

subset <- final_data[1:2000, selected_columns]
View(subset)

# Plotting the subset using the pairs function
pairs(subset)

#3. Estimating Number of Clusters (k) ---- ########################################
#3.1 Manually: ideal for low dimensional data
plot(subset)

#3.2 Sum of Square
k_proj <- list()
for(i in 1:10){
  k_proj[[i]] <- kmeans(subset, i)
}
k_proj

# Calculate the Between SS/Total SS ratio (k=4)
betweenss_totalss_proj <- numeric(10)
for(i in 1:10){
  betweenss_totalss_proj[i] <- k_proj[[i]]$betweenss / k_proj[[i]]$totss
}

dev.off() # Fixes "Error in plot.new() : figure margins too large" if needed
plot(1:10, betweenss_totalss_proj, type='b', 
     ylab='Between SS/Total SS', xlab="Clusters (k)")


#3.3 Elbow Method (using "factoextra" package)
?fviz_nbclust
fviz_nbclust(subset,kmeans,"wss") #k=4 or k=3
fviz_nbclust(subset,kmeans,"gap_stat") #k=3 or k=8
fviz_nbclust(subset,kmeans,"silhouette") #k=4

#4. K-Means Clustering ---- ########################################
#4.1 Creating K-Means clusters
fit_k_proj <- kmeans(subset, 3)
fit_k_proj
str(fit_k_proj)
plot(subset, col = fit_k_proj$cluster)

#4.2 Extracting the centers and clusters from fit_k_proj
clusters <- fit_k_proj$cluster
centers <- fit_k_proj$centers

#4.3 Visualizing results of clustering
fviz_cluster(fit_k_proj, subset, ellipse.type = "norm")

#4.4 Analyzing using Silhouette Method
silhouette_proj<-silhouette(clusters,dist(subset))
View(silhouette_proj)

?fviz_silhouette
fviz_silhouette(silhouette_proj,
                main='Silhouette plot',
                palette = "jco"
)

#4.5: Calculating Davies-Bouldin Index - Measures the average similarity ratio 
# of each cluster with its most similar cluster.
install.packages("clusterSim")
library(clusterSim)

dbi_km <- index.DB(subset, fit_k_proj$cluster)$DB
print(dbi_km)

#5. Hcluster (Hierarchical Cluster) ---- ########################################
#5.1 Heat-map (built using dentagram)
subset_matrix_proj <- data.matrix(subset[1:30,])
?pheatmap
pheatmap(subset_matrix_proj, 
         main = "Pretty heatmap",
         cellwidth =  35,
         cellheight = 13,
         fontsize = 8,
         display_numbers = TRUE,
         cluster_row = TRUE,
         cluster_col = TRUE,
         cutree_col=3,
         cutree_rows=8)

#5.2 create model
?dist
?hclust
fit_hc_proj <- hclust(dist(subset[1:50,]),"ward.D2")

#5.3 getting the clusters
hc_clusters_proj <- cutree(fit_hc_proj,4)
hc_clusters_proj

#5.4 visualization
?rect.hclust
plot(fit_hc_proj)
rect.hclust(fit_hc_proj,k=4,border="red")

plot(subset[1:50,],col=hc_clusters_proj)

fviz_dend(as.dendrogram(fit_hc_proj),k=4, cex=0.8,
          type="phylogenic",
          phylo_layout="layout_as_tree")

#5.5 Calculating Davies-Bouldin Index (Full dataset)
fit_hc_proj_full <- hclust(dist(subset),"ward.D2")
hc_clusters_proj_full <- cutree(fit_hc_proj_full,4)

dbi_hc <- index.DB(subset, hc_clusters_proj_full)$DB
print(dbi_hc)

#5.6 Calculating Silhouette Score
silhouette_scores <- silhouette(hc_clusters_proj_full, dist(subset))

# Plotting silhouette scores
fviz_silhouette(silhouette_scores)

#5.7 Interpreting and Visualizing clusters
subset$cluster <- hc_clusters_proj_full

# Box plot for 'age' across clusters
ggplot(subset, aes(x = factor(cluster), y = age)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Cluster", x = "Cluster", y = "Age")

# Scatter plot for 'age' vs 'hours_per_week' colored by cluster
ggplot(subset, aes(x = age, y = hours_per_week, color = factor(cluster))) +
  geom_point() +
  labs(title = "Age vs Hours per Week by Cluster", x = "Age", y = "Hours per Week")

#6. Model-based (GMM - gaussian mixture model) ---- ########################## 
# Note: 'k' isn't selected in this model.
?Mclust
fit_m_proj <- Mclust(subset)
fit_m_proj

plot(fit_m_proj)

summary(fit_m_proj)

#6.1 Calculating Silhouette Score
silhouette_scores_gmm <- silhouette(fit_m_proj$classification, dist(subset))
fviz_silhouette(silhouette_scores_gmm)

#6.2 Calculating Davies-Bouldin Index
gmm_clusters <- fit_m_proj$classification
dbi_gmm <- index.DB(subset, gmm_clusters)$DB
print(dbi_gmm)

#7. Density Model (DBSCAN) ----- #############################
kNNdistplot(subset,k=4)
abline(h=0.85, col="red", lty=2)

?dbscan
fit_dbscan_proj<-dbscan(subset,eps=0.85,minPts=9)
g_proj<-fviz_cluster(fit_dbscan_proj, data = subset, 
                geom = "point", main = "DBSCAN Clustering")
g_proj

#7.1 Calculating avg. silhouette score
clusters_dbscan <- fit_dbscan_proj$cluster
silhouette_scores_dbscan <- silhouette(clusters_dbscan, dist(subset))
fviz_silhouette(silhouette_scores_dbscan)
avg_silhouette_score_dbscan <- mean(silhouette_scores_dbscan[, "sil_width"])
print(avg_silhouette_score_dbscan)

#7.2 Calculating Davies-Bouldin Index
dbi_dbscan <- index.DB(subset, clusters_dbscan)$DB
print(dbi_dbscan)

#8. Spectral Clustering ----- #############################
install.packages("kernlab")
library(kernlab)

# Performing Spectral Clustering
num_clusters <- 4  #Specifying the number of clusters
fit_spectral <- specc(as.matrix(subset), centers = num_clusters)

# Extracting cluster assignments
spectral_clusters <- as.numeric(fit_spectral)

# Checking dimensions
print(dim(subset))
print(length(spectral_clusters))

# Visualizing Spectral Clustering
fviz_cluster(list(data = subset, cluster = spectral_clusters), 
             geom = "point", main = "Spectral Clustering")

#8.1 Calculating average silhouette score
silhouette_scores_spectral <- silhouette(spectral_clusters, dist(subset))
fviz_silhouette(silhouette_scores_spectral)
avg_silhouette_score_spectral <- mean(silhouette_scores_spectral[, "sil_width"])
print(avg_silhouette_score_spectral)

#8.2 Calculating Davies-Bouldin Index
dbi_spectral <- index.DB(subset, spectral_clusters)$DB
print(dbi_spectral)

#9: Final Comparison 
dbi_values <- list(
  "K-means" = dbi_km,
  "HClust" = dbi_hc,
  "Model Based (GMM)" = dbi_gmm,
  "DBSCAN" = dbi_dbscan,
  "Spectral Clustering" = dbi_spectral
)

for (i in names(dbi_values)) {
  print(paste(i, ":", dbi_values[[i]]))
}

