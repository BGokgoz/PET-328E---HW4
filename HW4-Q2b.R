# Loading libraries
library(cluster)
library(imputeMissings)

# Import the dataset
data <- SPE_shale  

# Removing rows with missing values
complete_data <- na.omit(data)

# Selecting the features (columns) for clustering
features <- complete_data[, c("Lease", "Well Number", "State", "Formation", "Initial Pressure Estimate (psi)", "Reservoir Temperature (deg F)", "Net Pay (ft)", "Porosity", "Water Saturation", "Oil Saturation", "Gas Saturation", "Gas Specific Gravity", "CO2", "N2", "TVD (ft)", "Spacing", "# Stages", "# Clusters", "# Clusters per Stage", "# of Total Proppant (MM Lbs)", "Lateral Length (ft)", "Top Perf (ft)", "Bottom Perf (ft)", "Sandface Temp (deg F)", "Static Wellhead Temp (deg F)", "Cumulative Gas Produced after 1 year, MCF")]

# Converting non-numeric columns to numeric
features[, c("Lease", "Well Number", "State", "Formation")] <- lapply(features[, c("Lease", "Well Number", "State", "Formation")], as.numeric)

# Imputing missing values
imputed_features <- impute(features)

# Scaling the features
scaled_features <- scale(imputed_features)

# Performing hierarchical clustering with minimum linkage
hc_result <- hclust(dist(scaled_features), method = "single")

# Plotting the dendogram
plot(hc_result, hang = -1)

# Determining a reasonable distance cut-off
rect.hclust(hc_result, h = 3.5)  

# Adding labels to the plot
labels <- paste(complete_data$Lease, complete_data$`Well Number`, sep = "_")
text(hc_result, labels = labels, hang = -1)
