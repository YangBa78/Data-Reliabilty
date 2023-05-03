library(vcdExtra)
data(ICU)
# reset the row numbering
rownames(ICU) <- NULL
m <- glm(died ~ age + uncons, data = ICU, family = binomial)
summary(m)
## 
## Call:
## glm(formula = died ~ age + uncons, family = binomial, data = ICU)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3128  -0.6231  -0.5140  -0.3226   2.4421  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -3.45771    0.80964  -4.271 1.95e-05 ***
## age          0.02778    0.01215   2.287   0.0222 *  
## unconsYes    3.58783    0.79350   4.522 6.14e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 200.16  on 199  degrees of freedom
## Residual deviance: 159.48  on 197  degrees of freedom
## AIC: 165.48
## 
## Number of Fisher Scoring iterations: 5


quantile(residuals(m))
##         0%        25%        50%        75%       100% 
## -2.3127570 -0.6231176 -0.5140270 -0.3225848  2.4420598


p_hat <- predict(m, type = "response")
y <- ifelse(ICU$died == "Yes", 1, 0)
e <- y - p_hat
cbind(y, p_hat, e) |> 
  head()
##   y      p_hat           e
## 1 0 0.06253121 -0.06253121
## 2 0 0.13962618 -0.13962618
## 3 0 0.21110650 -0.21110650
## 4 0 0.12375704 -0.12375704
## 5 0 0.26106920 -0.26106920
## 6 0 0.17645557 -0.17645557

e <- residuals(m, type = "response") 

r <- e / sqrt(p_hat * (1 - p_hat))
# or
r <- residuals(m, type = "pearson")
cbind(y, p_hat, r) |> 
  head()


d <- sign(e)*sqrt(-2*(y*log(p_hat) + (1 - y)*log(1 - p_hat)))
# or 
d <- residuals(m) 
cbind(y, p_hat, d) |> 
  head()
##   y      p_hat          d
## 1 0 0.06253121 -0.3593656
## 2 0 0.13962618 -0.5484311
## 3 0 0.21110650 -0.6886566
## 4 0 0.12375704 -0.5140270
## 5 0 0.26106920 -0.7778830
## 6 0 0.17645557 -0.6231176

