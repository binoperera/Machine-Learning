library(caret) #for data preparation, model building, and model evaluation library(leaps)# for ditacting most influential predictors for our model library(tidyverse)# this is opinionated collection
library(reshape2) 
library(ggplot2)# for visuaizations
library(ggcorrplot)# for visualizations library(MASS)
library(plotmo)# plot model surfaces library(corrplot)#detecting hidden patterns among variables library(kableExtra)#building common complex tables library(keras)
library(psych)
library(modelr)#creating elegant pipelines when modelling library(gridExtra)# working with the graphic objects library(Rmisc)#dtata anaizis and utility operations library(rpart)# building classifications
library(scales)# scalling the data library(cluster)
library(yardstick)# estimate model prorformances library(factoextra)# visualizing the output library(NbClust)#to determine relevent cluster in a data set
library(readxl) # reading file of the format with a xlsx  or xls  file into R


wine_data_set <- read_excel("d:/life/objective 1/Whitewine_v2.xlsx")

#step 1 =Scaling & Outlier Removal (free processing of the data) 
#accoding to 1.5* rule outliering and remove those data 
outliers = c()
for (i in 1:11 ) {# for loop for 1 to 11
stats = boxplot.stats(wine_data_set[[i]])$stats#satatus
rows_of_bottom_outlier = which(wine_data_set[[i]] < stats[1]) #rows for bottom outlier

rows_of_top_outlier = which(wine_data_set[[i]] > stats[5]) #rows for top outlier outliers = c(outliers ,rows_of_top_outlier[ !rows_of_top_outlier %in% outliers ] )#
 
outliers
outliers = c(outliers , rows_of_bottom_outlier[ !rows_of_bottom_outlier %in% outliers
] )#outliers
}


clean_wine_data_set = wine_data_set[-outliers, ]# wine data set oldpar = par(mfrow = c(2,6))#older path detecting
for (i in 1:12 ) {# for loop 1 to 12
truehist(clean_wine_data_set[[i]], xlab = names(clean_wine_data_set)[i], col = 'red', #clearing
}
 
main = paste("Average =", signif(mean(clean_wine_data_set[[i]]),3)))
 
par(oldpar)

dim(clean_wine_data_set) #after the removel of outliers reduces the size of dataset head(clean_wine_data_set)# heading




#dta type coverting

# normalize the large data
normalize_data <- sapply(clean_wine_data_set[,c(1,4,6,7,9,11,12)], function(x) (x - min(x))/(max(x) - min(x)))#normalization
normalize_data <- data.frame(normalize_data)#normalization head(normalize_data)# heading

completly_normalized data <- cbind(clean_wine_data_set[,c(2,3,5,8,10)],normalize_data)# completed normalized data
head(completly_normalized data)# heading


#number of cluster
# used NbClust for finding number of clusters

# 1. Euclidean Distance cluster_Available=NbClust(completly_normalized__data,distance="euclidean",
min.nc=2,max.nc=10,method="kmeans",index="all")#available clustering table(cluster_Available$Best.n[1,])#creating table barplot(table(cluster_Available$Best.n[1,]), # baplot
xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters (NbClust)")
cluster_ws <- 0
for (i in 1:15){# for loop for 1 to 15
cluster_ws[i] <- sum(kmeans(completly_normalized data, centers=i)$withinss)

}

plot(1:15,# plotting cluster_ws, type="b",
xlab="Number of Clusters",
ylab="Within groups sum of squares",sub = "Euclidean Distance")


# 2. Manhattan Distance\
cluster_Available=NbClust(completly_normalized data,distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")#clustering
table(cluster_Available$Best.n[1,])#creating table barplot(table(cluster_Available$Best.n[1,]), #barploting
xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters /NbClust")
cluster_ws <- 0
for (i in 1:15){#for 1 to 15
cluster_ws[i] <- sum(kmeans(completly_normalized data, centers=i)$withinss)

}
plot(1:15, cluster_ws, type="b",
xlab="Number of Clusters",
ylab="Within groups sum of squares",sub = "Manhattan Distance")


# 3. Elbow method
fviz_nbclust(completly_normalized data, kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 2) + # adding line for better visualization

labs(subtitle = "Elbow method") # adding subtitle for code


# 4. Silhouette method
fviz_nbclust(completly_normalized data, kmeans, method = "silhouette")+ labs(subtitle = "Silhouette method")# 4. Silhouette method

# clustering section 

# k equals 2
custer_2 <- kmeans(completly_normalized data, 2, iter.max = 140 , algorithm="Lloyd", nstart=100)# clustering for k2
custer_2
p.r <- pam(completly_normalized  data, 2, metric = c("euclidean"))# res data # Visualize
fviz_cluster(p.r)# clusterr


# k equals 3
custer_3 <- kmeans(completly_normalized data, 3, iter.max = 140 , algorithm="Lloyd", nstart=100)# clustering for k3
custer_3
p.r <- pam(completly_normalized  data, 3, metric = c("euclidean"))# res data # Visualize
fviz_cluster(p.r)# clusterr


# k equals 4
custer_4 <- kmeans(completly_normalized data, 4, iter.max = 140 , algorithm="Lloyd", nstart=100)# clustering for k4
custer_4
p.r <- pam(completly_normalized data, 4, metric = c("euclidean"))#res data

# Visualization of the data fviz_cluster(p.r)

#PCA section

clean_wine_data_set_pca <- prcomp(clean_wine_data_set[,c(1:11)], center =
TRUE,scale. = TRUE)# getting value for the pca summary(clean_wine_data_set_pca)#summarying

str(clean_wine_data_set_pca)#str

#Removing outliers from the PCA Dataset pca_dset = wine_data_set[9:12]# pca dataset

# outliers for pca dataset is checking here
zscore_for_pca = as.data.frame(sapply(pca_dset, function(pca_dset)(abs(pca_dset - mean(pca_dset))/sd(pca_dset))))#function for data frame
pca_dset_with_removed_outliers = zscore_for_pca[!rowSums(zscore_for_pca>3),]


#visualising pca dataset with outlines removal old_par0 = par(mfrow = c(2,6))
for ( i in 1:12 ) {#for loop for 1 to 12 boxplot(pca_dset_with_removed_outliers[[i]])#boxploting mtext(names(pca_dset_with_removed_outliers)[i], cex = 0.8, side = 1, line = 2)
}
par(old_par0)

# Scale the PCA dataset
# main data scalling proccess

pca scalled_data= as.data.frame(pca_dset_with_removed_outliers)#data scalling
#Display the scalled data
pca scalled_data
dim(pca_data_scalled)
####Cluster proccess for the PCA dataset Dataset for K == 2


set.seed(123)
pca_k2 = kmeans(pca scalled_data, centers = 2 , nstart = 25) pca_k2$cluster

# Centroid Plot against 1st 2 discriminant functions clusplot
#(clean_wine_data_set, km$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

fviz_cluster(pca_k2, data = pca_data_scalled, ellipse.type = "convex",
palette = "jco",
ggtheme = theme_minimal())

fviz_cluster(list(data = pca_data_scalled, cluster = km$cluster),

ellipse.type = "norm", geom = "point", stand = FALSE, palette = "jco", ggtheme = theme_classic())

pam.pca_res <- pam(pca_data_scalled, 2) # Visualize
fviz_cluster(pam.pca_res)

confusion_matrix = table(pca_data_scalled$quality,pca_k2$cluster)#confustion matrix confusion_matrix
#visualization of matrix

pca_quality = as.numeric(factor(pca_data_scalled$quality )) confusionMatrix(as.factor(c(as.factor(pca_k2$cluster))) ,
 as.factor(pca_quality))


pca_k2 <- kmeans(pca_data_scalled, 2, iter.max = 140 , algorithm="Lloyd", nstart=100) pca_k2
pca_k2$totss




pca_dset_with_removed_outliers