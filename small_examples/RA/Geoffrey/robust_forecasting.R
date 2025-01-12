devtools::load_all()
library(tidyverse)
library(readr)
library(magrittr)
library(forecast)

# test of auto.airma forecasting

# Function to replace lag columns with corresponding const values using row-by-row iteration
replace_dates_rowwise <- function(data) {
  # Create a lookup vector for y and const
  lookup <- setNames(data$const, data$y)

  # Identify lag columns (e.g., ly1, ly2, ...)
  lag_cols <- grep("^ly", names(data), value = TRUE)

  #convert columns to date times so that they can read in the const and numerics

  # Iterate through rows
  for (i in 1:nrow(data)) {
    for (col in lag_cols) {
      # Replace lagged date with corresponding const value
      date_val <- as.character(data[i, col]) # Get the date as a string
      if (!is.na(date_val)) { # If the date is not NA
        val <- as.numeric(lookup[date_val])
        data[i, col] <- val# Replace with the corresponding const
      }
    }
  }

  #convert everything to numerics, we do this because the values replacing the years are converted to date time because that is the current
  # data type of the data frame, so now we convert from dates to numerics to get our replaced numerical values
  # this is janky and maybe should be refactored to a better approach in the future to improve code readability
  data[, grepl("^ly", names(data))] <- lapply(data[, grepl("^ly", names(data))], function(x) as.numeric(x))

  return(data)
}

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

  browser()

  #retrieve and store the time column
  #select the time column
  time <- df %>% select("y")

  # Extract the last date from the existing column
  last_date <- max(time$y)

  # Generate the next `H` dates, incrementing by one day, this will need to be updated to handle quarterly increments
  forecast_horizion_times <- seq(last_date + 1, by = "month", length.out = H)

  for(i in 1:lag){
    df[[paste0("ly",i)]] <- lag(df$const,i)
  }
  browser()
  #df <- replace_dates_rowwise(df)

  df <- df[-c(1:lag),]
  x0 <- as.matrix(df$const)

  #model estimation
  df <- df[,c(2:ncol(df))]
  # Apply the function
  x1 <- as.matrix(df) #convert everything to numerics
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
  browser()
  df2 <- df %>% select("const") #make it so it is a data frame
  for(h in 1:H){
    df3 <- df2
    for(i in 1:lag){
      df3[[paste0("ly",i)]] <- lag(df3$const,i)
    }
    browser()
    x2 <- as.matrix(df3)
    value <- as.data.frame(c(1,sum(x2[(nrow(x2)+1-W):(nrow(x2)),1]),sum(x2[(nrow(x2)-W):(nrow(x2)-1),1]),x2[nrow(x2),]))
    forecasted_value <- (parw %*% c(1,sum(x2[(nrow(x2)+1-W):(nrow(x2)),1]),sum(x2[(nrow(x2)-W):(nrow(x2)-1),1]),x2[nrow(x2),]))
    #update df2
    df2 <- rbind(df2,data.frame("const" = forecasted_value))
    #convert df2 back to df to perform df operations

  }

  #combine the forecasts with the time stamps
  browser()

  forecasted_values <- df2[c((nrow(df)+1):nrow(df2)),]

  forecasts <- cbind(data.frame(forecast_horizion_times),data.frame(forecasted_values))

  return(forecasts)
  #df2[c((nrow(df)+1):nrow(df2)),]

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
  browser()
  #df2[c((nrow(df)+1):nrow(df2)),]
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
trend = FALSE  # linear trend?



data <- read.csv("small_examples/RA/Geoffrey/PCEPI.csv")

data <- data %>% dplyr::mutate(DATE = as.Date(.data$DATE))


forecasts1 <- srpredictor1(data,lag,trend,W,H)
forecasts2 <- srpredictor2(data,lag,trend,W,H)
forecasts3 <- srw(data,lag,W,H)

cbind(forecasts1,forecasts2,forecasts3)




