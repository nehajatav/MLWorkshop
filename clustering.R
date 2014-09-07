# create dataset
A = matrix(c(17, 24, 24, 10, 35, 22, 27, 34, 19, 29, 19, 21, 20, 8, 34, 5, 38, 10, 36, 16, 19, 20, 15, 174, 9, 185, 22, 181, 29, 170, 25, 157, 19, 157, 28, 162, 48, 171, 42, 185, 34, 172, 172, 12, 155, 12, 161, 36, 167, 29, 173, 25, 189, 37, 189, 42, 182, 45, 176, 43, 171, 35, 182, 21, 182, 16, 189, 13, 189, 32, 173, 172, 170, 165, 166, 169, 165, 148, 159, 157, 157, 178, 168, 178, 190, 174, 190, 164, 188, 161),ncol=2,byrow=TRUE)
plot(A)

#run kmeans with 1 initialization
cl = kmeans(A,4,nstart=1)
plot(A,col=cl$cluster)

#run kmeans with >1 initializations
cl = kmeans(A,4,nstart=50)
plot(A,col=cl$cluster)

#function to draw an elbow plot
elbow_plot <- function(mydata){
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i,nstart=50)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

#draw elbow plot of the data set
elbow_plot(A)

#add outlier to dataset
A=rbind(A,c(100,100))
plot(A)
cl = kmeans(A,4,nstart=50)
plot(A,col=cl$cluster)

#elbow plot for the dataset with outlier
elbow_plot(A)