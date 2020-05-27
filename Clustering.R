#df<- read.csv("/Users/Dario/Documents/Data Science/Statistical learning/Young People Survey/responses.csv")
df <- read.csv("Documenti/DataScience/Statistical Models/Project/Data/responses.csv")

library(ggplot2)
library(readr)
library(corrplot)
library(RColorBrewer)
library(stringr)    
library(NbClust)
library(cluster)

#formatting columns names
old <- names(df)
new <- str_replace_all(old, "\\.", "_") #replace the point
new <- str_replace_all(new, "\\__", "_or_") #format 
new <- str_replace_all(new, "\\__", "_") #format
colnames(df) <- new
attach(df)

#dividing into subsets 
df.music <- data.frame(df[0:19])
df.movies <- data.frame(df[20:31])
df.hobbies <- data.frame(df[32:63])
df.phobias <- data.frame(df[64:73])
df.health <- data.frame(df[74:76])
df.personality <- data.frame(df[77:133])

df.personality$Punctuality <- as.factor(df.personality$Punctuality)
df.personality$Lying <- as.factor(df.personality$Lying)
df.personality$Internet_usage <- as.factor(df.personality$Internet_usage)

levels(Punctuality)
levels(Lying)
levels(Internet_usage)
#personality with na filled with median 
personality.nona <- df.personality[, -c(32, 33, 57)]  
pers <- colnames(personality.nona)

# for(i in 1:length(pers)){
#   personality.nona[is.na(personality.nona[,i]), i] <- median(personality.nona[,i], na.rm = TRUE)
# }

for(i in 1:length(pers)){
  personality.nona[is.na(personality.nona[,i]), i] <- 3
}


#we decided to go ahead with 3 

#the below are three dendogram obtained with four different linkage methods and correlation-based distance
par(mfrow=c(1,1))

hc.single <- hclust(as.dist(1-cor(personality.nona)), method = "single")
hc.complete <- hclust(as.dist(1-cor(personality.nona)), method = "complete")
hc.average <- hclust(as.dist(1-cor(personality.nona)), method = "average")


hc.ward <- hclust(as.dist(1-cor(personality.nona)), method = "ward.D")
#we decided to go ahead with hc.ward and 5 clusters

plot(hc.ward, main = "Ward Linkage", xlab="", sub = "", cex=.9)
rect.hclust(hc.ward, k = 5, border = "orchid")
hc.ward$height
abline(h=hc.ward$height[length(hc.ward$height)-4],lty=2,col = "green")

ward.cut <- cutree(hc.ward, k=5)
table(ward.cut)

df[77:133] <- personality.nona

df["Cluster_1"] <- NA
df["Cluster_2"] <- NA
df["Cluster_3"] <- NA
df["Cluster_4"] <- NA
df["Cluster_5"] <- NA

cl_1 <- names(ward.cut[ward.cut==1])
cl_2 <- names(ward.cut[ward.cut==2])
cl_3 <- names(ward.cut[ward.cut==3])
cl_4 <- names(ward.cut[ward.cut==4])
cl_5 <- names(ward.cut[ward.cut==5])

df$Cluster_1 <- round(rowMeans(df[cl_1]),2)
df$Cluster_2 <- round(rowMeans(df[cl_2]),2)
df$Cluster_3<- round(rowMeans(df[cl_3]),2)
df$Cluster_4 <- round(rowMeans(df[cl_4]),2)
df$Cluster_5 <- round(rowMeans(df[cl_5]),2)

View(df)

# plot(hc.single, main = "single Linkage1", xlab="", sub = "", cex=.9)
# rect.hclust(hc.single, k = 3, border = "orchid")
# plot(hc.complete, main = "complete Linkage2", xlab="", sub = "", cex=.9)
# rect.hclust(hc.complete, k = 3, border = "orchid")
# 
# plot(hc.average, main = "average Linkage3", xlab="", sub = "", cex=.9)
# rect.hclust(hc.average, k = 3, border = "orchid")

#usiamo plot complete
#here we cut 3 cluster, after looking at the deindogram
# cut<- cutree(hc.complete,k = 3)
# cut <- data.frame(cut)
# table(cut)



#I would like to have a method to actually see the different heights of the branches, to see how close obs are 
"better with this visualization,no?" 
#yup

#I think that for our purpose the best distance is the correlation based ones
"yep"
#for the final paper: notice the high computational cost 


