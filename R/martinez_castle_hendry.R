martinez_castle_hendry_forecasting <- function(data,lag,trend,window,H) {

  #prepping data
  df <- data
  df$const <- 1
  if(trend==TRUE){
    df$tr <- 1:nrow(df)
    names(df) = c("y","const","trend")
  } else {
    names(df) = c("y","const")
  }
  for(i in 1:lag){
    df[[paste0("ly",i)]] <- lag(df$const,i)
  }
  df <- df[-c(1:lag),]
  x0 <- as.matrix(df$const)

  #model estimation
  x1 <- as.matrix(df[,c(2:ncol(df))])
  pars <- solve(t(x1) %*% x1) %*% (t(x1) %*% x0) #estimates

  #forecasting
  df2 <- df %>% select("const") #make it so it is a data frame
  for(h in 1:H){
    df3 <- df2
    if(trend==TRUE){
      parw2 <- c(pars[2],(1-sum(pars[c(3:nrow(pars))])^h)/window, sum(pars[c(3:nrow(pars))])^h+(1-sum(pars[c(3:nrow(pars))])^h)*(sum(pars[c(3:nrow(pars))]))/(1-sum(pars[c(3:nrow(pars))]))/window , -(1-sum(pars[c(3:nrow(pars))])^h)*(sum(pars[c(3:nrow(pars))]))/(1-sum(pars[c(3:nrow(pars))]))/window) #transformed estimates
    } else {
      parw2 <- c(0,(1-sum(pars[c(2:nrow(pars))])^h)/window, sum(pars[c(2:nrow(pars))])^h+(1-sum(pars[c(2:nrow(pars))])^h)*(sum(pars[c(2:nrow(pars))]))/(1-sum(pars[c(2:nrow(pars))]))/window , -(1-sum(pars[c(2:nrow(pars))])^h)*(sum(pars[c(2:nrow(pars))]))/(1-sum(pars[c(2:nrow(pars))]))/window) #transformed estimates
    }
    df3[[paste0("ly",1)]] <- lag(df3$const,1)
    x2 <- as.matrix(df3)
    forecasted_value <- (parw2 %*% c(1,sum(x2[(nrow(x1)+1-window):(nrow(x1)),1]),x2[nrow(x1),]))
    df2 <- rbind(df2,data.frame("const" = forecasted_value))
  }


  forecasted_values <- df2[c((nrow(df)+1):nrow(df2)),]
  return(forecasted_values)
} # Martinez Castle and Hendry (2022): Smooth Robust Predictor Generalized to multiple lags
