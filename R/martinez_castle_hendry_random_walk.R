martinez_castle_hendry_rw_forecasting <- function(data,lag,window,H) {

  #prepping data
  df <- data
  df$const <- 1
  names(df) = c("y","const")
  for(i in 1:lag){
    df[[paste0("ly",i)]] <- lag(df$const,i)
  }
  df <- df[-c(1:lag),]
  x0 <- as.matrix(df$const)

  #model estimation
  x1 <- as.matrix(df[,c(2:ncol(df))])
  pars <- solve(t(x1) %*% x1) %*% (t(x1) %*% x0) #estimates
  parw <- c((1-sum(pars[c(2:nrow(pars))]))/window,pars[c(2:nrow(pars))]) #transformed estimates

  #forecasting
  df2 <- df %>% select("const") #make it so it is a data frame
  for(h in 1:H){
    df3 <- df2
    for(i in 1:lag){
      df3[[paste0("ly",i)]] <- lag(df3$const,i)
    }
    x2 <- as.matrix(df3)
    forecasted_value <-(parw %*% c(sum(x2[(nrow(x1)+1-window):(nrow(x1)),1]),x2[nrow(x2),]))
    df2 <- rbind(df2,data.frame("const" = forecasted_value))
  }
  forecasted_value <-  df2[c((nrow(df)+1):nrow(df2)),]
  return(forecasted_value)
} # Martinez Castle and Hendry (2022): Smooth Random Walk Generalized to multiple lags
