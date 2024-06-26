# Import libraries
library(readxl)
library(dplyr)

# Read Excel data
data <- read_excel("Whitewine_v6.xlsx")

# Check for missing values
check_for_missing_values <- any(is.na(data))
print(paste("Missing values check:", check_for_missing_values))

# Remove the quality column
data <- data[, -ncol(data)]

# Summarize the data
summary(data)
boxplot(data,col="yellow")
# Function to remove outliers using IQR and replace with NA if necessary
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)] <- NA
  return(x)
}

# Identify numeric columns
numeric_columns <- sapply(data, is.numeric)

# Apply the function to numeric columns
data[numeric_columns] <- lapply(data[numeric_columns], remove_outliers)

# Handle NA values by removing rows with NA
data <- na.omit(data)

# Check for missing values after outlier removal and NA handling
check_for_missing_values_after_removal <- any(is.na(data))
print(paste("Missing values check after outlier removal and NA handling:", check_for_missing_values_after_removal))

# Summarize the data after outlier removal and NA handling
summary(data)

# Perform PCA
pca_wine <- prcomp(data, center = TRUE, scale = TRUE)

# Eigenvalues and eigenvectors from PCA
eigenvalues <- pca_wine$sdev^2
eigenvectors <- pca_wine$rotation
eigenvalues
eigenvectors

# Summarize the PCA results
summary(pca_wine)


# Extract eigenvalues
eigenvalues <- pca_wine$sdev^2
print(eigenvalues)

# Extract eigenvectors
eigenvectors <- pca_wine$rotation
print(eigenvectors)



# Extract variance explained by each PC
variance_explained <- summary(pca_wine)$sdev^2

# Calculate cumulative variance explained
cumulative_variance <- cumsum(variance_explained) / sum(variance_explained)

# Find the number of PCs needed to explain at least 85% variance
num_pcs <- which(cumulative_variance >= 0.85)[1]

# Print the number of PCs needed
print(paste("Number of PCs needed to explain at least 85% variance:", num_pcs))

# Extract the PCs needed
selected_pcs <- pca_wine$x[, 1:num_pcs]

# Print the selected PCs
print(selected_pcs)


wine_transform = as.data.frame(selected_pcs)
wine_transform


# NbClust
library(NbClust)

nb_results <- NbClust(wine_transform ,distance="euclidean", min.nc=2,max.nc=10, method = "kmeans", index = "all")
nb_results

# Elbow Method
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_fit <- kmeans(wine_transform, centers = i)
  wcss[i] <- kmeans_fit$tot.withinss
}
plot(1:10, wcss, type = "b", xlab = "Number of Clusters",
     ylab = "Within-Cluster Sum of Squares (WCSS)", main ="Elbow Method")

#silhouette
fviz_nbclust(wine_transform, kmeans, method = 'silhouette')
#gapstat
fviz_nbclust(wine_transform, kmeans, method = 'gap_stat')


k = 2
kmeans_wine = kmeans(wine_transform, centers = k, nstart = 10)
kmeans_wine



# WSS, BSS, TSS, BSS/TSS
WSS <- sum(kmeans_wine$withinss)
BSS <- kmeans_wine$betweenss
TSS <- sum(apply(dfNorm, 2, var)) * (nrow(dfNorm) - 1)
BSS_TSS <- BSS / TSS

cat("WSS:", WSS, "\nBSS:", BSS, "\nTSS:", TSS, "\nBSS/TSS:", BSS_TSS)

fviz_cluster(kmeans_wine, data = wine_transform)

fviz_cluster(kmeans_wine, data = wine_transform, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
library(fpc)
plotcluster(wine_transform, kmeans_wine$cluster)

library(cluster)

# silhouette score
sil <- silhouette(kmeans_wine$cluster, dist(wine_transform))
fviz_silhouette(sil)

#k means with k=3
k = 3
kmeans_wine = kmeans(wine_transform, centers = k, nstart = 10)
kmeans_wine
sil <- silhouette(kmeans_wine$cluster, dist(wine_transform))
fviz_silhouette(sil)

library(fpc) # for calinhara function

fviz_ch <- function(data) {
  ch <- c()
  for (i in 2:10) {
    km <- kmeans(data, i) # perform clustering
    ch[i] <- calinhara(data, # data
                       km$cluster, # cluster assignments
                       cn=max(km$cluster) # total cluster number
    )
  }
  ch <-ch[2:10]
  k <- 2:10
  plot(k, ch,xlab =  "Cluster number k",
       ylab = "Caliński - Harabasz Score",
       main = "Caliński - Harabasz Plot", cex.main=1,
       col = "dodgerblue1", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(ch==max(ch)) + 1, lwd=1, col="red", lty="dashed")
}

fviz_ch(wine_transform)


clenzi_score<- calinhara(wine_transform, kmeans_wine$cluster, cn=k)
clenzi_score

