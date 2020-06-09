df.cols <- read.csv("/Users/Dario/Documents/Data Science/Statistical learning/Young People Survey/columns.csv")
df.responses <- read.csv("/Users/Dario/Documents/Data Science/Statistical learning/Young People Survey/responses.csv")


library(MASS)
birthwt1 <- birthwt
summary(birthwt1)
head(birthwt1$race)
typeof(birthwt1$race)     

birthwt1$race <- as.factor(birthwt1$race)
str(birthwt1)


"ClustOfVar"
"spiegazione sommaria, fino a dove sono riuscito a capire"

#1 installiamo il package
install.packages("ClustOfVar")
library(ClustOfVar)


#2 creiamo il triz(=tree),

head(df.personality)
X.quanti <- df.personality[-c(32, 33, 57)]
triz <- hclustvar(X.quanti)
plot(triz)
cut.dend<- fviz_dend(triz, main=NULL, 
                     cex = .5,  
                     lwd = 0.4,
                     k = 5, 
                     color_labels_by_k = TRUE, 
                     rect = T, 
                     k_colors = c("#FC4E07", "#E7B800", "#00AFBB", "#f08200", "#74b358"), horiz = T)
cut.dend
#utilizza come misura di dissimilarity the lost of homogeneity observed when the two clusters A and B are merged. (pag.5)

#The user can use the stability function in order to have an idea of the stability of the tree. 
#ho capito che serve a scegliere i cluster. non ho ben capito cosa è.


#++++ATTENZIONE+++++
#PRIMA DI RUNNARE IL SEGUENTE CODICE FARE ATTENZIONE A QUEL B=40. indica i bootstrap samples che vengono effettuati.
#ovvero credo faccia fino ad altri 40 bootstrap dataset.
#utilizzane uno basso. sennò ci vuole del tempo.

stab <- stability(triz,B=40)


plot(stab, main="Stability of the partitions")
#dal plot si osserva che 5 è un buon numero di cluster. altrimenti ne avremmo dovuto scegliere "troppi" .

head(stab$matCR)
#stessa cosa in boxplot
boxplot(stab$matCR, main="Dispersion of the ajusted Rand index")

P3<-cutreevar(triz,5)
cluster <- P3$cluster
princomp(X.quanti[,which(cluster==1)],cor=TRUE)$sdev^2
princomp(X.quanti[,which(cluster==2)],cor=TRUE)$sdev^2
princomp(X.quanti[,which(cluster==3)],cor=TRUE)$sdev^2
princomp(X.quanti[,which(cluster==4)],cor=TRUE)$sdev^2
princomp(X.quanti[,which(cluster==5)],cor=TRUE)$sdev^2



P3<-cutreevar(tree,3,matsim=TRUE)
print(P3)


P3$wss
P3$sim #computational expensive: similarity matrix (da NULL perché non l'ha calcolata(se ho capito bene))
P3$cluster #to which cluster each variable belongs


P3$var
"questa l'abbiamo rivista, la prima sono i loadings^2. essendo quantitative i load^2 sono  corr^2, e accanto c'è la misura di correlazione"
#qui si vede piuttosto bene che le prime sono quelle che meglio identificano il cluster, e che in effetti hanno una più alta correlazione,
#poi mano a mano che si scende, sia di correlazione sia di load^2, perdono di significatività

head(P3$scores,50)



"devo rileggermelo bene, non ho capito come usarla, credo stia tutto nella ultima matrice, quella P3$scores, leggi anche gli esempi che aiutano"