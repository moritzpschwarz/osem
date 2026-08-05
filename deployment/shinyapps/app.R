library(shiny)
library(osem)
library(DT)
library(zoo)
library(forecast)
library(ggraph)
library(tidygraph)

readRDS("AT_model.rds") -> model
osem_app(model)
