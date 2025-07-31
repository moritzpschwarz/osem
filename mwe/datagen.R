# generate cointegrated system: Y, Z -> X (vector)
# setup
library(tidyverse)
nobs <- 101 # lose 1 obs
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
  data_cvar[, t] <- A %*% data_cvar[, t - 1] + mu + epsilont
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
  data_uni[, t] <- Phi %*% data_uni[, t - 1] + epsilont
}
data_uni <- as.data.frame(t(data_uni)) |>
  mutate(time = 1:nobs)
library(ggplot2)
ggplot(data = data_uni) +
  geom_line(aes(x = time, y = U), color = "blue") +
  geom_line(aes(x = time, y = V), color = "red") +
  geom_line(aes(x = time, y = W), color = "green")

# generate (i) pure AR process (unconnected to rest) and (ii) process with purely exogenous regressor
data_ar <- matrix(NA, nrow = 1, ncol = nobs)
rownames(data_ar) <- "Q"
data_ar[, 1] <- 1.5
for (t in 2:nobs) {
  # update observation Q
  data_ar[, t] <- 1 + 0.3 * data_ar[, t - 1] + rnorm(1)
}
data_other <- data.frame(
  Q = t(data_ar),
  R = rnorm(nobs)
) |>
  mutate(
    S = 5 * R + rnorm(nobs),
    time = 1:nobs
  )
ggplot(data = data_other) +
  geom_line(aes(x = time, y = Q), color = "blue") +
  geom_line(aes(x = time, y = R), color = "red") +
  geom_line(aes(x = time, y = S), color = "orange")

# save data
library(tidyverse)
data_cvar |>
  dplyr::select(Y, Z) |>
  bind_cols(data_uni |> dplyr::select(U, V, W)) |>
  bind_cols(data_other |> dplyr::select(Q, R, S)) |>
  # generate a variable based on variables from both CVAR and stationary system: Mt = 0.1Yt-1 + 0.1Ut-1
  mutate(error = rnorm(nobs)) |>
  mutate(M = 0.1 * lag(Y) + 0.1 * lag(U) + error) |>
  dplyr::select(-error) |>
  drop_na() |>
  mutate(time = seq.Date(from = "1900-01-01", by = "quarter", length.out = nobs - 1)) |>
  saveRDS(file = "./mwe/artificial_data.rds")

data_cvar |>
  dplyr::select(Y, Z) |>
  bind_cols(data_uni |> dplyr::select(U, V, W)) |>
  bind_cols(data_other |> dplyr::select(Q, R, S)) |>
  # generate a variable based on variables from both CVAR and stationary system: Mt = 0.1Yt-1 + 0.1Ut-1
  mutate(error = rnorm(nobs)) |>
  mutate(M = 0.1 * lag(Y) + 0.1 * lag(U) + error) |>
  dplyr::select(-error) |>
  drop_na() |>
  mutate(
    A = rnorm(nobs - 1),
    B = rnorm(nobs - 1)
  ) %>%
  mutate(time = seq.Date(from = as.Date("1900-01-01"), by = "quarter", length.out = nobs - 1)) %>%
  pivot_longer(cols = 1:11, names_to = "na_item", values_to = "values") %>%
  dplyr::mutate(geo = "DE") %>%
  saveRDS(file = "./tests/testthat/testdata/cvar/artificial_cvar_data.rds")
