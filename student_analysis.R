# Load relevant packages
library(corrplot)
library(ggcorrplot)

# Load in data
students <- read.csv("/Users/hannah/Desktop/gresa/Gresa.csv")

# Removing unnecessary columns
students <- students[, -c(1, 22)]

# Replace "Na" with NA
students[students == "Na"] <- NA

# Convert all columns to numeric
students <- data.frame(lapply(students, function(x) as.numeric(as.character(x))))

# Get general summary of data
summary(students)

# Analysis 1: Histograms
## Purpose: Checks the distribution of responses for each concern
par(mfrow = c(4, 5))  
par(mar = c(2, 2, 2, 2))  

for (i in 1:ncol(students)) {
  hist(students[[i]], main=colnames(students)[i], xlab="Rating", col="#f06434", breaks=10)
}

par(mfrow = c(1,1)) 

# Analysis 2: Correlation matrix
## Purpose: Identifies potentially correlated concerns
cor_matrix <- cor(students, use="complete.obs")
ggcorrplot(cor_matrix, 
           hc.order = TRUE,      
           type = "upper",       
           lab = TRUE,           
           lab_size = 3,       
           colors = c("red", "white", "blue"), 
           outline.color = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Analysis 3: K-means clustering
## Purpose: Helps identify groups of worries (ie. academic concerns, social concerns etc.)
### Deal with NA values
students <- data.frame(lapply(students, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(as.character(x))  
  } else {
    x
  }
}))

students <- data.frame(lapply(students, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}))

### Scale the data
students_scaled <- scale(students)

### Use elbow method to find optimal k
wss <- numeric(10) 
for (k in 1:10) {
  wss[k] <- sum(kmeans(students_scaled, centers = k, nstart = 25)$tot.withinss)
}

### Plot elbow curve
plot(1:10, wss, type = "b", pch = 19, main = "Elbow Method for Choosing K",
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares")

### k = 3 based on this curve
### Set seed for reproducibility
set.seed(123)

### Run K-means clustering with k = 3
kmeans_result <- kmeans(students_scaled, centers = 3, nstart = 25)

### Add cluster assignments to the original dataset
students$Cluster <- as.factor(kmeans_result$cluster)

### View cluster counts
table(students$Cluster)

### Cluster 1 = 53 students, Cluster 2 = 75 students, Cluster 3 = 11 students
### Cluster characterstics to be summarised to understand the unique traits of each group
cluster_summary <- aggregate(students[, -ncol(students)], 
                             by = list(Cluster = students$Cluster), 
                             FUN = mean)
print(cluster_summary)

### Table reveals:
### Cluster 1 students have moderate levels of concern overall, but are most concerned about academic-related issues (eg. homework) and socail issues (eg. losing old friends)
### Cluster 2 students have the least concern about transitioning schools
### Cluster 3 students are highly anxious about nearly every aspect of school transition

## Visualising the clusters using PCA visualisation
### Performing PCA
pca_result <- prcomp(students_scaled, center = TRUE, scale. = TRUE)

### Creating df for visualization
pca_data <- as.data.frame(pca_result$x)
pca_data$Cluster <- students$Cluster  

### Plotting clusters
library(ggplot2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  ggtitle("K-Means Clustering Visualization (PCA)") +
  theme_minimal()


