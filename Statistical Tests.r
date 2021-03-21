?wilcox.test

head(mtcars)

wilcox.test(x = mtcars$disp, y = mtcars$mpg)

#As the pvalue is less than 0.05, the two variables are dependent

head(mtcars)

median(x = mtcars$mpg)

DescTools::SignTest(x = mtcars$mpg, y = mtcars$disp, mu = 19.2)

#p-value is <0.05 so we reject the null hypothesis and conclude that the two variables are dependent

shapiro.test(mtcars$mpg)

#The null hypothesis is that the sample is normally distributed. 
#Since the p value is >0.05, we accept the null hypothesis and conclude that the sample is normally distributed

hist(mtcars$mpg)

set.seed(0)
ks.test(x = rnorm(100), y = rnorm(100))

#Null hypothesis is that they are from the same distribution. 
#According to the results, the accept the null hypothesis and conclude that they are from the same distribution.

set.seed(0)
ks.test(x = rnorm(100), y = runif(100))

var.test(x = mtcars$vs, y = mtcars$am)

#Null hypothesis is that the variance is the same

var.test(x = mtcars$vs, y = mtcars$disp)

#Null hypothesis is that the variance is the same

TeaTasting <-
matrix(c(3, 1, 1, 3),
       nrow = 2,
       dimnames = list(Guess = c("Milk", "Tea"),
                       Truth = c("Milk", "Tea")))

TeaTasting

fisher.test(TeaTasting)


