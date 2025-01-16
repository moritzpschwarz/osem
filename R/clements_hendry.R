#' Clements Hendry robust forecasting method
#'
#'
#' @param data
#' @param lag
#' @param trend
#' @param window
#' @param H
#'
#' @inheritParams forecast_exogenous_values
#'
#' @return a list of forecasted values
#'
#'
#'
clements_hendry_forecasting <- function(data,lag,trend,window,H) {

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
  df <- df[,c(2:ncol(df))]
  # Apply the function

  x1 <- as.matrix(df) #convert everything to numerics

  #test for matrix singularity
  lhs <- t(x1) %*% x1
  rhs <- t(x1) %*% x0

  if (det(lhs) < 1e-8) {
    stop("Matrix is near-singular. implmenet a regularization factor or alter exogenous dataset.")
  }

  pars <- solve(lhs,rhs) #solve(t(x1) %*% x1) %*% (t(x1) %*% x0) #estimates <- this can be prone in non-invertible matrices

  if(trend==TRUE){
    if(lag>1){
      parw <- c(pars[2],1/window,-1*sum(pars[c(3:nrow(pars))])/window,sum(pars[c(3:nrow(pars))]),0,0*c(1:(lag-1))) #transformed estimates
    } else {
      parw <- c(pars[2],1/window,-1*sum(pars[c(3:nrow(pars))])/window,sum(pars[c(3:nrow(pars))]),0) #transformed estimates
    }

  } else {
    if(lag>1){
      parw <- c(0,1/window,-1*sum(pars[c(2:nrow(pars))])/window,sum(pars[c(2:nrow(pars))]),0,0*c(1:(lag-1))) #transformed estimates
    } else {
      parw <- c(0,1/window,-1*sum(pars[c(2:nrow(pars))])/window,sum(pars[c(2:nrow(pars))]),0) #transformed estimates
    }
  }

  #forecasting
  df2 <- df %>% select("const") #make it so it is a data frame
  for(h in 1:H){
    df3 <- df2
    for(i in 1:lag){
      df3[[paste0("ly",i)]] <- lag(df3$const,i)
    }

    x2 <- as.matrix(df3)
    forecasted_value <- (parw %*% c(1,sum(x2[(nrow(x2)+1-window):(nrow(x2)),1]),sum(x2[(nrow(x2)-window):(nrow(x2)-1),1]),x2[nrow(x2),]))
    #update df2
    df2 <- rbind(df2,data.frame("const" = forecasted_value))
    #convert df2 back to df to perform df operations

  }

  forecasted_values <- df2[c((nrow(df)+1):nrow(df2)),] #extract forecasted values

  return(forecasted_values)

} # Castle Clements and Hendry (2015)
