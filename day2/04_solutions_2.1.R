##########################################
## Introduction to R: solutions 2.1
## author: PBRC: DA Hughes
## date: Oct 16th, 2024
##########################################

## In this R Script we will be looking at a simulated data set
## called "simulated_data.txt" and answer some question about
## it as described in the file 03_practical_2.1.html

###-------read in the data set --------------
mydata = read.delim("simulated_data.txt")

###-------1. Is the data a data frame? --------------
is.data.frame(mydata)

# Answer: TRUE

###-------2. How many individuals are in the data set ? --------------
nrow(mydata)

# Answer: 500


###-------3. How many rows are in the data set? --------------
ncol(mydata)


###-------4. How many features ("f__") are in the data set? --------------
colnames(mydata) 

# Answer: 14


#### a specific count
length( grep("f", colnames(mydata)) )
# Answer: 10

#### an alternative way in case the letter "f" is in other variable names
## extract the first character of each string in colnames
first_letter = substr(colnames(mydata),1,1)
length( grep( "f", first_letter ) )

# Answer: 10

###-------5. Estimate the number of missing values for each variable? --------------
missingness = apply(mydata, 2, function(x){sum(is.na(x))})
missingness

# Answer: 
# sex    age height weight   f1     f2     f3     f4     f5     f6     f7     f8     f9    f10 
# 0      0      0      0     25     10      0      5      0     25      5     25     10      0


###-------6. What is the Pearson's r (correlation) between height and weight? --------------
cor.test(mydata$height, mydata$weight, method = "p")

# Answer: 0.755

###-------7. How much of the variance in weight is explained by height? --------------
r = cor.test(mydata$height, mydata$weight, method = "p")$estimate
r^2

# Answer: 57%

###-------8. Sex is numeric in your data frame. Make it a factor. --------------
mydata$sex = as.factor(mydata$sex)


###-------9. Is there a sex effect on height? --------------
fit = lm(height ~ sex, data = mydata )
summary(fit)

# Answer: yes


#### Extract just the coefficient table
summary(fit)$coef
# coef(summary(fit)) ## This also yields the same result
# coef(summary(fit))[2,] ## Or to extract just the sex summary stats


###-------10. Make a box plot of the effect of sex on height. --------------
boxplot(height ~ sex,
        data = mydata, 
        xlab = "sex", 
        col = c("red2","blue2"),
        main = "The effect of sex on height")


###-------11. Write this box plot as a pdf to your results directory. --------------
pdf("../results/sex_height_boxplot.pdf", width = 3, height = 5)
boxplot(height ~ sex,
        data = mydata, 
        xlab = "sex", 
        col = c("red2","blue2"),
        main = "The effect of sex on height")
dev.off()




###-------12. Estimate a correlation matrix of all the features among themselves (m-x-m matrix) --------------
f = grep( "f", first_letter )
Cmat = cor(mydata[, f], use = "pairwise.complete.obs", method = "pearson")


###-------13. Write this matrix to your results directory. --------------
write.table(Cmat, file = "../results/feature_cor_matrix.txt", 
            row.names = TRUE, 
            col.names = TRUE,
            sep = "\t", 
            quote = FALSE)


###-------14. Estimate BMI and add it to the data set. --------------
mydata$bmi = mydata$weight / mydata$height^2



###-------15. What is the mean BMI in the data set? --------------
mean(mydata$bmi)

# Answer: 29.58286

###-------16. What is the mean BMI for males (1) and females (0) --------------
## Females
mean(mydata$bmi[mydata$sex == 0])
# Answer: 29.19524

## Males
mean(mydata$bmi[mydata$sex == 1])
# Answer: 30.1453

###-------17. What is the range (min, max) observed? --------------
range(mydata$bmi)
## These also work
# min(mydata$bmi)
# max(mydata$bmi)

# Answer: 22.58880 37.19958

###-------18. What is the 95% CI of the BMI distribtion? --------------
quantile(mydata$bmi, probs = c(0.025, 0.975))

# Answer:
#    2.5%    97.5% 
#  24.81429 34.67452

###-------19. Is there an association between BMI and the first feature (f1)? --------------
fit = lm(f1 ~ bmi, data = mydata)
summary(fit)$coef

# Answer: NO

###-------20. Fit a multivariable model  --------------
# define f1 as the dependent (outcome) variable 
# and sex, age, and BMI as the independent variables.

fit = lm(f1 ~ sex + age + bmi, data = mydata)
summary(fit)$coef


###-------21. Is there an association between sex, age, and BMI, on f1?  --------------

# Answer:
# There is no effect, or association of sex, age, or BMI on f1. 

###-------22. Avoid repetition  --------------

# Can you write a loop, (s)apply function, or your own function 
# to return a table of the beta, se, and p values for sex, age, and BMI 
# on each feature in the data set? 
# Include the sample size of each analysis as well. 

sumstats = c()
f_cols = grep( "f", first_letter )

for(i in f_cols ){
  fit = lm(mydata[,i] ~ sex + age + bmi, data = mydata)
  ## feature id
  fid = colnames(mydata)[i]
  names(fid) = "fid"
  ## sample size
  n = length(fit$residuals)
  names(n) = "n"
  ## coefficients
  coef = summary(fit)$coef
  ## sex sum stats
  s = coef[2,c(1,2,4)]
  names(s) = paste0("sex_", c("beta","se","p"))
  ## age sum stats
  a = coef[2,c(1,2,4)]
  names(a) = paste0("age_", c("beta","se","p"))
  ## bmi sum stats
  b = coef[2,c(1,2,4)]
  names(b) = paste0("bmi_", c("beta","se","p"))
  ## data out
  out = c(fid, n, s, a, b)
  sumstats = rbind(sumstats, out)
}

## turn into a data frame
sumstats = as.data.frame(sumstats)

## make numeric column numeric
for(i in 2:ncol(sumstats)){
  sumstats[,i] = as.numeric(sumstats[,i])
}


###-------Another way  --------------
sumstats = c()
features = colnames(mydata)[f_cols]

for(f in features){
  form = as.formula(paste(f, "~ sex + age + bmi"))
  fit = lm(form, data = mydata)
  ## feature id
  fid = f
  names(fid) = "fid"
  ## sample size
  n = length(fit$residuals)
  names(n) = "n"
  ## coefficients
  coef = summary(fit)$coef
  ## sex sum stats
  s = coef[2,c(1,2,4)]
  names(s) = paste0("sex_", c("beta","se","p"))
  ## age sum stats
  a = coef[2,c(1,2,4)]
  names(a) = paste0("age_", c("beta","se","p"))
  ## bmi sum stats
  b = coef[2,c(1,2,4)]
  names(b) = paste0("bmi_", c("beta","se","p"))
  ## data out
  out = c(fid, n, s, a, b)
  sumstats = rbind(sumstats, out)
}

## turn into a data frame
sumstats = as.data.frame(sumstats)

## make numeric column numeric
for(i in 2:ncol(sumstats)){
  sumstats[,i] = as.numeric(sumstats[,i])
}


###-------A third way  --------------
features = colnames(mydata)[f_cols]

sumstats = sapply(features, function(f){
  form = as.formula(paste(f, "~ sex + age + bmi"))
  fit = lm(form, data = mydata)
  ## feature id
  fid = f
  names(fid) = "fid"
  ## sample size
  n = length(fit$residuals)
  names(n) = "n"
  ## coefficients
  coef = summary(fit)$coef
  ## sex sum stats
  s = coef[2,c(1,2,4)]
  names(s) = paste0("sex_", c("beta","se","p"))
  ## age sum stats
  a = coef[2,c(1,2,4)]
  names(a) = paste0("age_", c("beta","se","p"))
  ## bmi sum stats
  b = coef[2,c(1,2,4)]
  names(b) = paste0("bmi_", c("beta","se","p"))
  ## data out
  out = c(fid, n, s, a, b)
  return(out)
})

## transpose the output
sumstats = t(sumstats)

## turn into a data frame
sumstats = as.data.frame(sumstats)

## make numeric column numeric
for(i in 2:ncol(sumstats)){
  sumstats[,i] = as.numeric(sumstats[,i])
}



###-------23. How many of the features have a BMI effect / association?  --------------
sum( sumstats$bmi_p < 0.05/10 )

# Answer: 2


###-------24. Which features have an assocation with BMI?  --------------
w = which( sumstats$bmi_p < 0.05/10 )
sumstats$fid[w]

# Answer: "f6" "f9"


#### A quick look at the associations
sumstats[w, c(1,2,9:11)]


###-------25. Write this table to your results directory  --------------
write.table(sumstats, file = "../results/association_sumstat.txt",
            row.names = FALSE,
            col.names = TRUE, 
            sep = "\t",
            quote = FALSE)


