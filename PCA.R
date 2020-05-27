"Pca on Movies, Music, Spending habits"
df <- read.csv("/Users/Dario/Documents/Data Science/Statistical learning/Young People Survey/responses.csv")

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

"now we can work on PCA"
 

head(df.movies)
str(df.movies)
summary(df.movies)

# 1st variable is about "if you enjoy watching movies", i would like to see the result with it and without it.
#the na's will be replaced with the median
df.movies <- data.frame(df[21:31])

pref <- colnames(df.movies)

for(i in 1:length(pref)){
  df.movies[is.na(df.movies[,i]), i] <- median(df.movies[,i], na.rm = TRUE)
}

summary(df.movies)
#no more na(niceee)
"PCA FOR MOVIES"

#to see the variances
apply(df.movies,2,var) #2 for columns,1 for rows

#scaling them--> standard deviation one
pr.out <- prcomp(df.movies,scale= TRUE)
names(pr.out)

pr.out$center #means
pr.out$scale  #standard deviation
pr.out$rotation #principle component loading vector

dim(pr.out$x)

biplot(pr.out,scale= 0)

pr.out$sdev # standard deviation of each principle component

pr.var <- pr.out$sdev^2 #to obtain the variance explained by each principal component
pr.var

pve <- pr.var/sum(pr.var) #compute the proportion of variance explained by each principal component
pve

plot(pve,xlab= "Principal COmponent", ylim=c(0,1),type = 'b') #elbow
plot(cumsum(pve),xlab = "Principal COmponent", ylim=c(0,1),type = 'b') #ogiva, o cumulative sum

#i risultati sono piuttosto MEH
names(df.movies)
summary(df.movies)

"PCA FOR Music"

df.music
summary(df.music)

#again filling na
pref <- colnames(df.music)
for(i in 1:length(pref)){
  df.music[is.na(df.music[,i]), i] <- median(df.music[,i], na.rm = TRUE)
}
summary(df.music)

pr.music <- prcomp(df.music, scale=TRUE)
names(pr.music)

biplot(pr.music,scale=0)

pr.varmusic <- pr.music$sdev^2
pr.varmusic

pve.music <- pr.varmusic/sum(pr.varmusic)
pve.music

plot(pve.music,xlab= "Principal COmponent", ylim=c(0,1),type = 'b') #elbow
plot(cumsum(pve.music),xlab = "Principal COmponent", ylim=c(0,1),type = 'b') #ogiva, o cumulative sum


#anche qui MEH

"PCA for spending habits"


df.spending <- data.frame(df[134:140])
names(df.spending)
summary(df.spending)
#dealing with na
pref <- colnames(df.spending)
for(i in 1:length(pref)){
  df.spending[is.na(df.spending[,i]), i] <- median(df.spending[,i], na.rm = TRUE)
}
summary(df.spending)

pr.spending <- prcomp(df.spending,scale=TRUE)
biplot(pr.spending,scale=0)

pr.varspending <- pr.spending$sdev^2
pr.varspending

pve.spending <- pr.varspending/sum(pr.varspending)
pve.spending

plot(pve.spending,xlab= "Principal COmponent", ylim=c(0,1),type = 'b') #elbow
plot(cumsum(pve.spending),xlab = "Principal COmponent", ylim=c(0,1),type = 'b') #ogiva, o cumulative sum

#meglio, 4 variabili spiegano 80%

"PCA PER Hobbies"

summary(df.hobbies)
#ma che lo scrivo a fare
pref <- colnames(df.hobbies)
for(i in 1:length(pref)){
  df.hobbies[is.na(df.hobbies[,i]), i] <- median(df.hobbies[,i], na.rm = TRUE)
}

summary(df.hobbies)

pr.hobbies <- prcomp(df.hobbies,scale=TRUE)
biplot(pr.hobbies,scale=0)

pr.varhobbies <- pr.hobbies$sdev^2
pr.varhobbies

pve.hobbies <- pr.varhobbies/sum(pr.varhobbies)
pve.hobbies

plot(pve.hobbies,xlab= "Principal COmponent", ylim=c(0,1),type = 'b') #elbow
plot(cumsum(pve.hobbies),xlab = "Principal COmponent", ylim=c(0,1),type = 'b') #ogiva, o cumulative sum

#..... forse non è inutile da 30 passeremmo a 15 che spiegano l' 80%
