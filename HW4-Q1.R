# Loading libraries
library(cluster)

# Importing the dataset
data <- SPE_shale 

# Selecting only the numerical features for clustering
numeric_features <- data[, c("Initial Pressure Estimate (psi)", "Reservoir Temperature (deg F)", "Net Pay (ft)", "Porosity", "Water Saturation", "Oil Saturation", "Gas Saturation", "Gas Specific Gravity", "CO2", "N2", "TVD (ft)", "Spacing", "# Stages", "# Clusters", "# Clusters per Stage", "# of Total Proppant (MM Lbs)", "Lateral Length (ft)", "Top Perf (ft)", "Bottom Perf (ft)", "Sandface Temp (deg F)", "Static Wellhead Temp (deg F)", "Cumulative Gas Produced after 1 year, MCF")]

# Scaling the features
scaled_features <- scale(numeric_features)

# Setting the maximum number of clusters
max_clusters <- 10

# Creating an empty vector to store the WCSS values
wcss <- vector("numeric", max_clusters)

# Performing K-means clustering for different numbers of clusters
for (k in 1:max_clusters) {
  # Run K-means clustering
  kmeans_result <- kmeans(scaled_features, centers = k, nstart = 10)
  
  # Calculate the WCSS (total within-cluster sum of squares)
  wcss[k] <- kmeans_result$tot.withinss
}

# Plotting the WCSS values
plot(1:max_clusters, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "WCSS")

# Applying the elbow method to determine the optimal number of clusters
elbow_point <- 0
for (k in 2:(max_clusters - 1)) {
  # Calculating the difference in WCSS between adjacent clusters
  diff_wcss <- wcss[k] - wcss[k-1]
  
  # Checking if the difference exceeds a certain threshold (e.g., 10%)
  if (diff_wcss <= 0.1 * wcss[k-1]) {
    elbow_point <- k
    break
  }
}

# Highlighting the elbow point on the plot
points(elbow_point, wcss[elbow_point], col = "red", pch = 19)

# Setting the optimal number of clusters
optimal_clusters <- elbow_point

# Running K-means clustering with the optimal number of clusters
kmeans_result <- kmeans(scaled_features, centers = optimal_clusters, nstart = 10)

# Getting the cluster assignments
cluster_assignments <- kmeans_result$cluster

# Printing the cluster assignments
print(cluster_assignments)
