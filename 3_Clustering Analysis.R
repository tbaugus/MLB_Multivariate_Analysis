#Install needed packages
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("GGally", dependencies=TRUE, repos='https://cloud.r-project.org/')
#install.packages("dplyr)
#install.packages("vctrs")
#install.packages("MASS")  # for isoMDS function

### Load Libraries 
library(dplyr)
library(ggplot2)
library(MASS)  # for isoMDS function
#update.packages(ask = FALSE)

# set working directory and outpaths 
setwd("C:/Users/remcy/Desktop/CyzFoldz/ApplicationtoSchools/TexasTech/2023 - Fall Classes/ISQS 6350 - Multivariate Analysis/Project/Dataset/Clean/")
filepath = "https://raw.githubusercontent.com/Slyth3/Multivariate-Analysis-Baseball-Hitting-Statistics/main/cleandata.csv"
key_cols = "./key_cols.csv"

# Set/create folder
outpath = "/4_outfiles"

if (!dir.exists(paste0(getwd(),outpath))){
  dir.create(paste0(getwd(),outpath))
}else{
  print("dir exists")
}

df = read.csv(filepath, row.names = "Name")
head(df)
txt_cols <- df%>% select("Team", "outlier")
team <- txt_cols$Team
num_cols <- df[ , c(2:104)]

# Scale data
df_s <- scale(num_cols)
df_s

#Calculate and plot the Euclidean distance
dist <- dist(df_s)
#plot(dist, xlab = "Eucledian Distance", ylab = "Batters", main="Euclidean Distance Between Batters")

#Multidimensional scaling (MDS)Plot for the Euclidean distance representation
set.seed(123)
mds_dist <- cmdscale(dist)
plot(mds_dist, type = "n",
     main = 'MDS Plot - Euclidean Distances (Equivalent to PCA")',
     xlab = "First Component(PC1)", ylab = "Second Component(PC2)")
text(mds_dist, labels = row.names(df), col = "orange", cex=1.2)

set.seed(123)
mds_result <- isoMDS(dist_matrix)
plot(mds_result$points, main = "MDS Plot of Euclidean Distances")

df.hc <- hclust(dist, "complete")
plot(df.hc, cex=0.5, main = "MLB Hierarchical Clustering Dendogram - Complete", xlab = "Eucledian Distance")
abline(h=24)

# #heights at which each clustering occurred
# round(df.hc$height, 3)
# plot(rev(df.hc$height))
# 
# #Which observation in each cluster. cut the clusters at 3
# ct <- cutree(df.hc, 3)
# ct
# 
# print_table_with_title <- function(table_obj, title) {
#   cat(paste(title, ":\n"))
#   print(table_obj)
# }
# 
# table(ct) #how many observations go in each cluster
# 
# c1 <- subset((df$Team), ct == 1)
# #c1
# print_table_with_title(c1,"Cluster #1")
# c2 <- subset((df$Team), ct == 2)
# table(c2)
# print_table_with_title(c1,"Cluster #2")
# c3 <- subset((df$Team), ct == 3)
# table(c3)
# print_table_with_title(c3,"Cluster #3")
# #c3
# 
# #look at z-scores.
# index1 <- match(c1, df$Team) #
# #round(colMeans(df_s[index1,]), 2) # players in this cluster are... high in pull.perc(0.23 and LA...Do for the remaining clusters
# 
# 
# # Calculate rounded column means
# rounded_means <- round(colMeans(df_s[index1, ]), 2)
# rounded_means
# # Order the columns based on rounded means
# ordered_columns <- desc(rounded_means)
# ordered_columns
# #need to order them by the highest z-score.

#K-Means Clustering
set.seed(123)
km <- kmeans(df_s, centers = 3)

#Calculate WGSS without iterations
clust<-table(km$cluster)
km$tot.withinss # sum of the square distances from the points to their cluster centroids, to the mean of each cluster. did we improve?

#try and choose key variables and analyse them later
round(subset(df_s, km$cluster == 1), 3)

#Calculate WGSS with 20 iterations
set.seed(123)
km <- kmeans(df_s, centers = 3, nstart = 20)
km$tot.withinss

#look at the best number of clusters to use for our k-means. and use it to being with in order to determine which number to use.

plot.wgss <- function(df, maxc){
  wss <- numeric(maxc)
  for (i in 1:maxc){
    wss[i] <- kmeans(df, iter.max = 100,
                     centers = i, nstart = 20)$tot.withinss
  }
  plot(1:maxc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within Groups Sum of Squares",
       main = "MLB Scree Plot")
  return(wss)
}
#with 3 clusters, some improvements (37396.28 -37326.74 = 69.54)

# Call the function and store the returned wss values
wss_values <- plot.wgss(df_s, 30)

abline(h = wss_values, lty = 2, col = c(rep('#0000ff', 2), '#ffa500', rep('#0000ff', 3)))

# Generate cluster IDs from cluster_levels. This will help us be able to plot our data by clusters through cluster_ID
cluster_assignments <- km$cluster
cluster_levels <- unique(cluster_assignments)
df_s$cluster_id <- as.factor(cluster_assignments)

# Create a list to store datapoints for each cluster
datapoints_per_cluster <- vector("list", length = length(cluster_levels))

# Loop through clusters and extract datapoints for each cluster
for (i in seq_along(cluster_levels)) {
  cluster_id <- cluster_levels[i]
  datapoints_per_cluster[[i]] <- df_s[cluster_assignments == cluster_id, ]
}

# Print or access individual clusters and their datapoints
for (i in seq_along(cluster_levels)) {
  cat("Cluster ID:", cluster_levels[i], "\n")
  print(datapoints_per_cluster[[i]])
  cat("\n")
}

#Cluster levels homeruns and plate appearances
df_s <- scale(num_cols)
df_s_pa_hr <- df[, c("plate.appearances", "home.runs")]
set.seed(123)

df_s_pa_hr$team <- df$Team

km.df_s_pa_hr <- kmeans(df_s_pa_hr, centers = 3, nstart = 20)
km.df_s_pa_hr
cluster_assignments <- km.df_s_pa_hr$cluster
cluster_assignments
# Add cluster ID to the dataframe
df_s_pa_hr$cluster_id <- as.factor(cluster_assignments)
df_s_pa_hr$cluster_id
# Plot with base R plot function
plot(df_s_pa_hr$plate.appearances, df_s_pa_hr$home.runs,col = as.numeric(df_s_pa_hr$cluster_id), xlab = "Plate Appearance", ylab = "Home runs")

#cluster#1 values
table(cluster_assignments)

###########################################################################################################################

#Cluster levels batting average and homeruns
df_s_pa_hr <- df[, c("plate.appearances", "home.runs")]
df_s_pa_hr <- scale(as.numeric(df_s_pa_hr))
df_s_pa_hr
set.seed(123)

km.df_s_pa_hr <- kmeans(df_s_pa_hr, centers = 3, nstart = 20)
km.df_s_pa_hr$size
cluster_assignments <- km.df_s_pa_hr$cluster
cluster_assignments
# Add cluster ID to the dataframe
df_s_pa_hr$cluster_id <- as.factor(cluster_assignments)
df_s_pa_hr$cluster_id

#clusters plotting with og values
missing_values <- is.na(df_s_pa_hr$plate.appearances) | is.na(df_s_pa_hr$home.runs) | !is.finite(df_s_pa_hr$plate.appearances) | !is.finite(df_s_pa_hr$home.runs)

# Filter out rows with missing or non-finite values
filtered_data <- df_s_pa_hr[!missing_values, ]

# Plot the filtered data with xlim explicitly set
cluster_colors <- c("green","blue", "orange")
plot(filtered_data$plate.appearances, filtered_data$home.runs, col = cluster_colors[as.numeric(filtered_data$cluster_id)], 
     xlab = "Batting Average", ylab = "Home runs", main="Plate Appearance and Home Runs Clusters",
     xlim = c(min(filtered_data$plate.appearances), max(filtered_data$plate.appearances)))
text(df_s_pa_hr$plate.appearances, df_s_pa_hr$home.runs, labels = df_s_pa_hr$team, pos = 3, col = cluster_colors)
legend("topright", legend = unique(filtered_data$cluster_id), col = cluster_colors, pch = 16, title = "Cluster ID")

# Same previous plot with Teams labels on each data point.
cluster_colors <- c("green","blue", "orange")
plot(filtered_data$plate.appearances, filtered_data$home.runs, col = cluster_colors[as.numeric(filtered_data$cluster_id)], 
     xlab = "Plate Appearance", ylab = "Home runs", main="Plate Appearance and Home Runs Clusters - With Players Teams",
     xlim = c(min(filtered_data$plate.appearances), max(filtered_data$plate.appearances)))
text(df_s_pa_hr$plate.appearances, df_s_pa_hr$home.runs, labels = df_s_pa_hr$team, pos = 3, col = cluster_colors)
legend("topright", legend = unique(filtered_data$cluster_id), col = cluster_colors, pch = 16, title = "Cluster ID")

#Clusters plotting with z-score values.
#Calculate z-scores for plate.appearances and home.runs
z_scores <- scale(filtered_data[, c("plate.appearances", "home.runs")])
table(z_scores)

#z_score tables for each cluster for plate.appearances and home.runs
zscore_cluster1 <- round(subset(z_scores, km.df_s_pa_hr$cluster == 1), 3)
head(zscore_cluster1, 20)
tail(zscore_cluster1, 20)

# Remove leading and trailing whitespaces from row names
row_names_zscore_cluster1 <- row.names(zscore_cluster1)
row_names_zscore_cluster1 <- trimws(row_names_zscore_cluster1)

# Check for common row names

common_row_names <- intersect(rownames(zscore_cluster1), row.names(txt_cols))

# Extract the 'Team' column from the original data frame (df) for common row names
teams <- txt_cols$Team[row.names(txt_cols) %in% common_row_names]
teams
# Add the 'Team' column to the zscore_cluster1 data frame
zscore_cluster1_with_teams <- cbind(zscore_cluster1[common_row_names, , drop = FALSE], Team = teams)

# Display the head of the modified data frame
noquote (head(zscore_cluster1_with_teams, 20))
noquote (tail(zscore_cluster1_with_teams, 20))
zscore_cluster2 <- round(subset(z_scores, km.df_s_pa_hr$cluster == 2), 3)
head(zscore_cluster2, 20)
tail(zscore_cluster2, 20)
zscore_cluster3 <- round(subset(z_scores, km.df_s_pa_hr$cluster == 3), 3)
head(zscore_cluster3, 20)
tail(zscore_cluster3, 20)
# Plot the data using z-scores
plot(z_scores[, 1], z_scores[, 2], col = as.numeric(filtered_data$cluster_id), 
     xlab = "Z-Score (Plate Appearance)", ylab = "Z-Score (Home Runs)", main="Plate Appearances and Home Runs Clusters - Z_Scores")

########################################################### END ###################################################################