## Load the EMT package:
library(EMT)

## Input data for a three-dimensional case:
observed <- c(5,2,1)   		 
prob <- c(0.25, 0.5, 0.25) 	 

## Calculate p-value using default options:
out <- multinomial.test(observed, prob)        
# p.value = 0.0767

## Plot the probabilities for each event:
plotMultinom(out)

## Calculate p-value for the same input using Pearson's chisquare:
out <- multinomial.test(observed, prob, useChisq = TRUE)  
# p.value = 0.0596 ; not the same!

## Test the hypothesis that all sides of a dice have the same probabilities:
prob <- rep(1/6, 6)	                 
observed <- c(4, 5, 2, 7, 0, 1)			
out <- multinomial.test(observed, prob) 
# p.value = 0.0357 -> better get another dice!

# the same problem using a Monte Carlo approach:
if (FALSE) {
  out <- multinomial.test(observed, prob, MonteCarlo = TRUE, ntrial = 5e+6)   
}



# Simulate some data
n <- 100
set.seed(4)
x <- factor(sample(1:3, n, replace = TRUE))
y <- x[-(1:2)]
xlag1 <- x[-c(1,n)]
xlag2 <- x[-((n - 1):n)]

# Fit the null hypothesis
library(VGAM)
#> Loading required package: stats4
#> Loading required package: splines
mod0 <- vglm(y ~ xlag1, family = multinomial())
predictvglm(mod0, newdata = data.frame(xlag1 = factor(1:3)), type = "response")
#>           1         2         3
#> 1 0.4166667 0.3055556 0.2777778
#> 2 0.3636364 0.3636364 0.2727273
#> 3 0.3103448 0.3448276 0.3448276

# Fit the alternative
mod1 <- update(mod0, .~xlag1*xlag2)
predictvglm(mod1, newdata = expand.grid(xlag1 = factor(1:3),xlag2 = factor(1:3)), type = "response")
#>           1          2         3
#> 1 0.2000000 0.46666667 0.3333333
#> 2 0.3636364 0.18181818 0.4545455
#> 3 0.3333333 0.33333333 0.3333333
#> 4 0.7500000 0.08333333 0.1666667
#> 5 0.3333333 0.50000000 0.1666667
#> 6 0.4444444 0.33333333 0.2222222
#> 7 0.3333333 0.33333333 0.3333333
#> 8 0.4000000 0.40000000 0.2000000
#> 9 0.1818182 0.36363636 0.4545455

# Test the null against the alternative
anova(mod0, mod1, test="LRT", type = "I")
#> Analysis of Deviance Table
#> 
#> Model 1: y ~ xlag1
#> Model 2: y ~ xlag1 + xlag2 + xlag1:xlag2
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       190     213.56                     
#> 2       178     198.24 12   15.322   0.2243

# Testing conditional independence as suggested Ben via log-linear models
tables <- table(y,xlag2,xlag1)
tables
#> , , xlag1 = 1
#> 
#>    xlag2
#> y   1 2 3
#>   1 3 9 3
#>   2 7 1 3
#>   3 5 2 3
#> 
#> , , xlag1 = 2
#> 
#>    xlag2
#> y   1 2 3
#>   1 4 4 4
#>   2 2 6 4
#>   3 5 2 2
#> 
#> , , xlag1 = 3
#> 
#>    xlag2
#> y   1 2 3
#>   1 3 4 2
#>   2 3 3 4
#>   3 3 2 5
library(MASS)
loglin(tables, list(c(1,3),c(2,3)))


install.packages('heplots')
library(heplots)
data(iris)
boxM(Sepal.Length ~ Species, data = iris)


# Define the observed frequencies
observed <- c(12, 20, 18)/50

# Define the expected frequencies
expected <- c(0.3, 0.4, 0.3)

# Perform the multinomial test
chisq <- sum((observed - expected)^2 / expected)
df <- length(observed) - 1
pvalue <- pchisq(chisq, df, lower.tail = FALSE)

# Print the results
cat("Chi-squared statistic:", chisq, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", pvalue, "\n")


