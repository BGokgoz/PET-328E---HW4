library(dplyr)
library(FNN)
library(dbscan)
library(randomForest)

# Loading the dataset
data <- SPE_shale

# Selecting the columns for anomaly detection
columns_for_detection <- c('Initial Pressure Estimate (psi)', 'Reservoir Temperature (deg F)',
                           'Net Pay (ft)', 'Porosity', 'Water Saturation', 'Oil Saturation',
                           'Gas Saturation', 'Gas Specific Gravity', 'CO2', 'N2',
                           'TVD (ft)', 'Spacing', '# Stages', '# Clusters',
                           '# Clusters per Stage', '# of Total Proppant (MM Lbs)',
                           'Lateral Length (ft)', 'Top Perf (ft)', 'Bottom Perf (ft)',
                           'Sandface Temp (deg F)', 'Static Wellhead Temp (deg F)',
                           'Cumulative Gas Produced after 1 year, MCF')

# Removing rows with missing values in the selected columns
data_filtered <- data[complete.cases(data[, columns_for_detection]), ]

# K-Nearest Neighbors with 5 neighbors
knn_5 <- get.knnx(data_filtered[, columns_for_detection], k = 5)
avg_distances_5 <- apply(knn_5$nn.dist, 1, mean)
threshold_5 <- mean(avg_distances_5) + 2 * sd(avg_distances_5)
anomalies_5 <- which(avg_distances_5 > threshold_5)
cat('Anomalies with 5 neighbors:', anomalies_5, '\n')

# K-Nearest Neighbors with 10 neighbors
knn_10 <- get.knnx(data_filtered[, columns_for_detection], k = 10)
avg_distances_10 <- apply(knn_10$nn.dist, 1, mean)
threshold_10 <- mean(avg_distances_10) + 2 * sd(avg_distances_10)
anomalies_10 <- which(avg_distances_10 > threshold_10)
cat('Anomalies with 10 neighbors:', anomalies_10, '\n')


