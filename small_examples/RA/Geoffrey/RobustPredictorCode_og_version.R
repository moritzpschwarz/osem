

setwd("/Users/geoffreyharper/Desktop/OSEM/exogenous_forecasting_techniques/Robust_techniques") #change as needed

rm(list=ls())


srpredictor1 <- function(data,lag,trend,W,H) {

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
    df[[paste0("ly",i)]] <- lag(df$y,i)
  }
  browser()
  df <- df[-c(1:lag),]
  x0 <- df$y
  #model estimation
  x1 <- as.matrix(df[,c(2:ncol(df))])
  pars <- solve(t(x1) %*% x1) %*% (t(x1) %*% x0) #estimates
  if(trend==TRUE){
    if(lag>1){
      parw <- c(pars[2],1/W,-1*sum(pars[c(3:nrow(pars))])/W,sum(pars[c(3:nrow(pars))]),0,0*c(1:(lag-1))) #transformed estimates
    } else {
      parw <- c(pars[2],1/W,-1*sum(pars[c(3:nrow(pars))])/W,sum(pars[c(3:nrow(pars))]),0) #transformed estimates
    }

  } else {
    if(lag>1){
      parw <- c(0,1/W,-1*sum(pars[c(2:nrow(pars))])/W,sum(pars[c(2:nrow(pars))]),0,0*c(1:(lag-1))) #transformed estimates
    } else {
      parw <- c(0,1/W,-1*sum(pars[c(2:nrow(pars))])/W,sum(pars[c(2:nrow(pars))]),0) #transformed estimates
    }
  }

  #forecasting
  df2 <- df[,1]
  for(h in 1:H){
    for(i in 1:lag){
      df2[[paste0("ly",i)]] <- lag(df2$y,i)
    }
    x2 <- as.matrix(df2)
    df2 <- rbind(df2[,1],(parw %*% c(1,sum(x2[(nrow(x2)+1-W):(nrow(x2)),1]),sum(x2[(nrow(x2)-W):(nrow(x2)-1),1]),x2[nrow(x2),]))[1])
  }
  df2[c((nrow(df)+1):nrow(df2)),]
} # Castle Clements and Hendry (2015)
srpredictor2 <- function(data,lag,trend,W,H) {

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
    df[[paste0("ly",i)]] <- lag(df$y,i)
  }
  df <- df[-c(1:lag),]
  x0 <- df$y

  #model estimation
  x1 <- as.matrix(df[,c(2:ncol(df))])
  pars <- solve(t(x1) %*% x1) %*% (t(x1) %*% x0) #estimates

  #forecasting
  df2 <- df[,1]
  for(h in 1:H){
    if(trend==TRUE){
      parw2 <- c(pars[2],(1-sum(pars[c(3:nrow(pars))])^h)/W , sum(pars[c(3:nrow(pars))])^h+(1-sum(pars[c(3:nrow(pars))])^h)*(sum(pars[c(3:nrow(pars))]))/(1-sum(pars[c(3:nrow(pars))]))/W , -(1-sum(pars[c(3:nrow(pars))])^h)*(sum(pars[c(3:nrow(pars))]))/(1-sum(pars[c(3:nrow(pars))]))/W) #transformed estimates
    } else {
      parw2 <- c(0      ,(1-sum(pars[c(2:nrow(pars))])^h)/W, sum(pars[c(2:nrow(pars))])^h+(1-sum(pars[c(2:nrow(pars))])^h)*(sum(pars[c(2:nrow(pars))]))/(1-sum(pars[c(2:nrow(pars))]))/W , -(1-sum(pars[c(2:nrow(pars))])^h)*(sum(pars[c(2:nrow(pars))]))/(1-sum(pars[c(2:nrow(pars))]))/W) #transformed estimates
    }
    df2[[paste0("ly",1)]] <- lag(df2$y,1)
    x2 <- as.matrix(df2)
    df2 <- rbind(df2[,1],(parw2 %*% c(1,sum(x2[(nrow(x1)+1-W):(nrow(x1)),1]),x2[nrow(x1),]))[1])
  }
  df2[c((nrow(df)+1):nrow(df2)),]
} # Martinez Castle and Hendry (2022): Smooth Robust Predictor Generalized to multiple lags
srw <- function(data,lag,W,H) {

  #prepping data
  df <- data
  df$const <- 1
  names(df) = c("y","const")
  for(i in 1:lag){
    df[[paste0("ly",i)]] <- lag(df$y,i)
  }
  df <- df[-c(1:lag),]
  x0 <- df$y

  #model estimation
  x1 <- as.matrix(df[,c(2:ncol(df))])
  pars <- solve(t(x1) %*% x1) %*% (t(x1) %*% x0) #estimates
  parw <- c((1-sum(pars[c(2:nrow(pars))]))/W,pars[c(2:nrow(pars))]) #transformed estimates

  #forecasting
  df2 <- df[,1]
  for(h in 1:H){
    for(i in 1:lag){
      df2[[paste0("ly",i)]] <- lag(df2$y,i)
    }
    x2 <- as.matrix(df2)
    df2 <- rbind(df2[,1],(parw %*% c(sum(x2[(nrow(x1)+1-W):(nrow(x1)),1]),x2[nrow(x2),c(1:(ncol(x2)-1))]))[1])
  }
  df2[c((nrow(df)+1):nrow(df2)),]
}                # Martinez Castle and Hendry (2022): Smooth Random Walk Generalized to multiple lags


lag <- 12     # number of lags
H <- 12      # number of forecast horizons
W <- 1       #  Local window smoothing - usually set equal to frequency of series
trend = TRUE  # linear trend?

library(tidyverse)

data <- read_csv("PCEPI.csv") #load data



forecasts1 <- srpredictor1(data,lag,trend,W,H)
forecasts2 <- srpredictor2(data,lag,trend,W,H)
forecasts3 <- srw(data,lag,W,H)

cbind(forecasts1,forecasts2,forecasts3)










