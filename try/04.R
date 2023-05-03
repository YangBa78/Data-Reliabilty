# curve

(40-sum(diag(table(newdt[newdt$workerid==31,]$trueLable, newdt[newdt$workerid==31,]$decision))))/40
(40-sum(diag(table(newdt[newdt$workerid==15,]$trueLable, newdt[newdt$workerid==15,]$decision))))/40
(40-sum(diag(table(newdt[newdt$workerid==27,]$trueLable, newdt[newdt$workerid==27,]$decision))))/40

table(newdtC[newdtC$workerid==32,]$trueLable, newdtC[newdtC$workerid==32,]$decision)



error_rate <- function(pred, actual) {
  error <- mean(pred != actual)
  return(error)
}

error_rate(newdt[newdt$workerid==31,]$trueLable, newdt[newdt$workerid==31,]$decision)

cumulative.error.rate <- function(decisions, actual) {
  error.count <- sum(decisions != actual)
  cumulative.error.rate <- error.count / length(decisions)
  return(cumulative.error.rate)
}

cumulative_error_rate <- function(pred, actual) {
  n <- length(pred)
  error <- cumsum(pred != actual)
  cumulative_error <- error / 1:n
  return(cumulative_error)
}

cumulative.error.rate(newdt[newdt$workerid==31,]$decision, newdt[newdt$workerid==31,]$trueLable)


n <- 100

# Set the desired proportion of each category
prop_1 <- 0.5
prop_0 <- 1 - prop_1

# Generate the binary variable
binary_var <- sample(c(0, 1), n, replace = FALSE, prob = c(prop_0, prop_1))

# Check the proportion of each category
table(binary_var) / n


# Generate a sequence of row indices
row_indices <- 1:n

# Randomly select 50% of the indices to be assigned 0
selected_indices <- sample(row_indices, size = floor(n/2), replace = FALSE)

# Create the binary variable with 0 for selected indices and 1 for the rest
binary_var <- numeric(n)
binary_var[selected_indices] <- 0
binary_var[-selected_indices] <- 1
table(binary_var)


x<-c(10, 12, 75, 89, 25, 100, 67, 89, 4, 67, 120.2, 140.5, 170.5, 78.1)
qq<-cut(x, breaks=seq(0,1, length.out=4), include.lowest=T, labels=F)
qq<-cut(x, breaks=quantile(x, seq(0,1, length.out=4)), include.lowest=T, labels=F) 
cbind(x, qq)

x <- c(12, 1, 25, 12, 65, 2, 6, 17)
c <- cut(x, breaks = 3)
levels(c) <- c("First", "Second", "Third")
c
65/3
