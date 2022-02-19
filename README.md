# DPAChandimal
#Clustering Analysis 

dis = dist(Brain_Size_Data[2:4], method="euclidean") #Euclidean distance between points

hcBrainAve = hclust(dis, method="ave") #Group average as similarity measure
hcBrainWard = hclust(dis, method="ward.D") #Ward's method as similarity measure
par(mfcol=c(1,2))
plot(hcBrainAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcBrainAve, k=2, border="green")
plot(hcBrainWard, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcBrainWard, k=2, border="green")
