df.cols <- read.csv("Documenti/DataScience/Statistical Models/Project/Data/columns.csv")
df.responses <- read.csv("Documenti/DataScience/Statistical Models/Project/Data/responses.csv")
library(ggplot2)
library(readr)
library(corrplot)
library(RColorBrewer)
library(stringr)    #formatting columns names
old <- names(df.responses)
new <- str_replace_all(old, "\\.", "_") #replace the point
new <- str_replace_all(new, "\\__", "_or_") #format 
new <- str_replace_all(new, "\\__", "_") #format
colnames(df.responses) <- new
attach(df.responses)

omit <- na.omit(df.responses)
dim(omit) 
dim(df.responses) - dim(omit)
#not a good idea to omit all na as we have a difference of 324 rows escluded, we have to decide how to treat those na

#for the preferences in music, movies and hobbies replace missing values with median of column
pref <- colnames(df.responses[0:63])
for(i in 1:length(pref)){
  df.responses[is.na(df.responses[,i]), i] <- median(df.responses[,i], na.rm = TRUE)
}

#remaining na:
na_count <-sapply(df.responses, function(y) sum(length(which(is.na(y)))))
na_df <- as.table(na_count, names(na_count))
na_df <- na_count[na_count!=0]
View(na_df)
names(na_df)

#no null or nan values

#dividing into subsets per category of question and visually exploring internal correlations
df.music <- data.frame(df.responses[0:19])
length(names(df.music))
corrplot(cor(df.music),
         type= "upper",
         method ="circle",
         col = brewer.pal(n = 5, name = "Spectral"),
         tl.cex = 0.5,
         tl.col = "olivedrab4",
         tl.srt = 45,
         mar = c(2, 2, 2, 2))

  
df.movies <- data.frame(df.responses[20:31])
length(names(df.movies))
names(df.movies)
corrplot(cor(df.movies),
         type= "upper",
         method ="circle",
         col = brewer.pal(n = 5, name = "Spectral"),
         tl.cex = 0.5,
         tl.col = "olivedrab4",
         tl.srt = 45,
         mar = c(2, 2, 2, 2))


df.hobbies <- data.frame(df.responses[32:63])

df.phobias <- data.frame(df.responses[64:73])


df.health <- data.frame(df.responses[74:76])


df.personality <- data.frame(df.responses[77:133])

df.personality$Punctuality <- as.factor(df.personality$Punctuality)
df.personality$Lying <- as.factor(df.personality$Lying)
df.personality$Internet_usage <- as.factor(df.personality$Internet_usage)

par(mfrow=c(1,1))
corrplot(cor(df.personality[, -c(32, 33, 57)], use = "complete.obs"), 
         type= "upper",
         method ="circle",
         col = brewer.pal(n = 5, name = "Spectral"),
         tl.cex = 0.5,
         tl.col = "olivedrab4",
         tl.srt = 45,
         mar = c(2, 2, 2, 2))

#personality with just numerical variables and nas filled with median
personality.nona <- df.personality[, -c(32, 33, 57)]  
pers <- colnames(personality.nona)
for(i in 1:length(pers)){
  personality.nona[is.na(personality.nona[,i]), i] <- median(personality.nona[,i], na.rm = TRUE)
}


df.spending <- data.frame(df.responses[134:140])
cor(df.spending[,-8], use = "complete.obs")
corrplot(cor(df.spending[,-8], use = "complete.obs"), 
         type= "upper",
         method ="circle",
         col = brewer.pal(n= 5, name = "Spectral"),
         tl.cex = 0.5,
         tl.col = "olivedrab4",
         tl.srt = 45,
         mar = c(2, 2, 2, 2))

df.demo <- data.frame(df.responses[140:150])
length(names(df.demo))




