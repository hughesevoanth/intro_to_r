##########################################
## Introduction to R: practical 2.2
## author: PBRC: DA Hughes
## date: Oct 16th, 2024
##########################################

## In this R Script we will be looking at a simulated data set
## called "simulated_data.txt". But I keep getting errors in 
## my code. Can you help me?

###-------read in the data set --------------
mydata = read.delim("simulated_data.txt")
mydata$sex = as.factor(mydata$sex)

###-------1. estimate means for all features --------------
first_letter = substr(colnames(mydata),1,1)
f = grep( "f", first_letter )
apply(mydata[,f], 2, mean )

## I got a bunch of NAs. What is Wrong??

### Try running just one feature.
mean(mydata$f1)

###-------2. Estimate standard deviations --------------
apply(mydata[,f], 2, function(x){
  stdev(x, na.rm = TRUE)
  } )


###-------3. Estimate minimum values --------------
apply(mydata[,g], 2, function(x){
  min(x, na.rm = TRUE)
} )


###-------4. I want to run a conditional loop --------------
my_values = c(1:5, NA, 7:10)
for(i in my_values ){
  if(i < 8){
    x = i+100
    cat(paste0( "Running element ", i, " works\n") )
  } else {
    x = i
  }
  cat(paste0( "\t x = ", x, "\n") )
}


###-------5. scale all the features in my data set --------------
## Z-transform my features and assign those values to a new R object called "ztrans_features"
ztrans_features = apply(mydata[, 5:15], 2, scale)



###-------6. Generate Principal Components for all my data --------------
pca = prcomp( mydata , center = TRUE, scale = TRUE)

    ## !! There are two errors in (6) above. Don't get discouraged!




### Some Extra Fun Stuff

## extract the variance explains
varexp = round( summary(pca)[[6]][2,1:2], d = 4 ) *100

## Plot the PCs
pcs = as.data.frame( pca$x[, 1:2] )

### Identify k of 4
pcs$k = as.factor( kmeans(pcs, centers = 4)$cluster )

### make the plot
plot(pcs[,1], pcs[,2], 
     xlab = paste0("PC1 ", varexp[1], "%"), 
     ylab = paste0("PC2 ", varexp[2], "%"), 
     pch = 19, col = pcs$k, main = "PCA of my data")


