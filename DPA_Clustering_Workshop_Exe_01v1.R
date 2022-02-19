help()
x=2
brainData = read.csv("Brain Size Data.csv", header = TRUE)
mean(brainData$Height)
median(brainData$Height)
mode(brainData$Height)
mymode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mymode(brainData$Height)
midrange = (max(brainData$Height) - min(brainData$Height))/2
midrange
mean(brainData$Weight)

median(brainData$Weight)

mode(brainData$Weight)

mymodew <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mymodew(brainData$Weight)
midrangew = (max(brainData$Weight) - min(brainData$Weight))/2
midrangew

mean(brainData$VIQ)

median(brainData$VIQ)
mode(brainData$VIQ)
mymodeviq <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mymodeviq(brainData$VIQ)
midrangeviq = (max(brainData$VIQ) - min(brainData$VIQ))/2
midrangeviq

mean(brainData$PIQ)

median(brainData$PIQ)
mode(brainData$PIQ)
mymodepiq <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mymodepiq(brainData$PIQ)
midrangepiq = (max(brainData$PIQ) - min(brainData$PIQ))/2
midrangepiq

mean(brainData$MRI_Count)

median(brainData$MRI_Count)
mode(brainData$MRI_Count)
mymodemricount <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mymodemricount(brainData$MRI_Count)
midrangemricount = (max(brainData$MRI_Count) - min(brainData$MRI_Count))/2
midrangemricount

plot(brainData)

dis = dist(Brain_Size_Data[2:4], method="euclidean") #Euclidean distance between points

hcBrainAve = hclust(dis, method="ave") #Group average as similarity measure
hcBrainWard = hclust(dis, method="ward.D") #Ward's method as similarity measure
par(mfcol=c(1,2))
plot(hcBrainAve, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcBrainAve, k=2, border="green")
plot(hcBrainWard, hang=-1, cex.main = 0.75, cex.axis = 0.5)
rect.hclust(hcBrainWard, k=2, border="green")



