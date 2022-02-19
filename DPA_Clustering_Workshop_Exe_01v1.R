help()
install.packages("party")
library(party)
head(wine)

wine.f=wine
wine.f$X1<- NULL

View(wine.f)

#Change column names to actual features of wine from the chemical analysis

colnames(wine)<-c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                  "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins",
                  "Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

colnames(wine.f)<-c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                  "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins",
                  "Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
View(wine)

wine.stand <- scale(wine.f[-1])
View(wine.stand)
results <- kmeans(wine.stand,3)
attributes(results)

results$centers

head(wine)
colnames(wine)<-c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                  "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins",
                  "Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

head(wine)

table(wine$Type, results$cluster)
plot(wine.f[,2:13])

dis = dist(wine.f[1:13], method="euclidean")

hcattrAve = hclust(dis, method="ave") 
hcattrWard = hclust(dis, method="ward.D")

par(mfcol=c(1,2))

plot(hcattrAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)

rect.hclust(hcattrAve, k=3, border="green")

plot(hcattrWard, hang=-1, cex.main = 0.75, cex.axis = 0.5)

rect.hclust(hcattrWard, k=3, border="green")

hcwineAveCut = cutree(hcattrAve, 3)
hcwineAveCut

hcattrWardcut = cutree(hcattrWard,3)
par(mfcol=c(1,2))
plot(wine.f[,1:2], col=hcwineAveCut, cex.main=0.75)
plot(wine.f[,1:2], col=hcwineAveCut, cex.main=0.75)

dis = dist(wine.f[1:13], method="euclidean")

hcattrWard = hclust(dis, method="ward.D") 
plot(hcattrWard, hang=-1, labels=wine$Type, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcattrWard, k=3, border="blue")

hcattrWardcut = cutree(hcattrWard, k=3)
table(wine$Type, hcattrWardcut)



