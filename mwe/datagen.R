# generate cointegrated system: Y, Z -> X (vector)
# setup
nobs <- 100
Sigma <- matrix(c(0.2, 0.1, 0.1, 0.1), nrow = 2, ncol = 2) # error matrix
A <- matrix(c(0.8, 0, 0.6, 1), nrow = 2, ncol = 2) # AR matrix
mu <- matrix(c(0.5, 0), nrow = 2, ncol = 1) # intercept
X0 <- matrix(c(5.5, 1), nrow = 2, ncol = 1) # initial values

library(MASS) # normal errors

# generate data
# store output
data_cvar <- matrix(NA, nrow = 2, ncol = nobs)
rownames(data_cvar) <- c("Y", "Z")
# store initial value
data_cvar[, 1] <- X0
# use loop to generate data
set.seed(123)
for (t in 2:nobs) {
  # generate errors
  epsilont <- matrix(mvrnorm(n = 1, mu = c(0, 0), Sigma = Sigma), nrow = 2, ncol = 1)
  # update observations of Y, Z
  data_cvar[, t] <- A %*% data_cvar[, t-1] + mu + epsilont
}
data_cvar <- as.data.frame(t(data_cvar)) |>
  mutate(time = 1:nobs)

library(ggplot2)
ggplot(data = data_cvar) +
  geom_line(aes(x = time, y = Y), color = "blue") +
  geom_line(aes(x = time, y = Z), color = "red")

# generate system without cycles but lagged relationship
# Wt = 0.5Wt-1 + 0.4Vt + Ut + epsilonWt
# Vt = 0.2Vt-1 + 0.5Wt-1 + 2Ut + epsilonVt
# Ut = 0.4Ut-1 + epsilonUt
B <- matrix(c(1, -2, -1, 0, 1, -0.4, 0, 0, 1), nrow = 3, ncol = 3)
A <- matrix(c(0.4, 0, 0, 0, 0.2, 0, 0, 0.5, 0.5), nrow = 3, ncol = 3)
Phi <- solve(B) %*% A # reduced form matrix
data_uni <- matrix(NA, nrow = 3, ncol = nobs)
rownames(data_uni) <- c("U", "V", "W")
data_uni[, 1] <- c(0, 0, 0) # initial values of zero (LR mean)
for (t in 2:nobs) {
  # generate errors
  epsilont <- matrix(mvrnorm(n = 1, mu = c(0, 0, 0), Sigma = diag(3)), nrow = 3, ncol = 1)
  # update observations of U, V, T
  data_uni[, t] <- Phi %*% data_uni[, t-1] + epsilont
}
data_uni <- as.data.frame(t(data_uni)) |>
  mutate(time = 1:nobs)
library(ggplot2)
ggplot(data = data_uni) +
  geom_line(aes(x = time, y = U), color = "blue") +
  geom_line(aes(x = time, y = V), color = "red") +
  geom_line(aes(x = time, y = W), color = "green")




