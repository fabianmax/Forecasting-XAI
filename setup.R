library(data.table)
library(dplyr)
library(lubridate)
library(reshape2)
library(vars)
library(caret)
library(iml)
library(DALEX)
library(breakDown)
library(iBreakDown)
library(ingredients)
library(drifter)
library(ggplot2)
library(plotly)
library(ggridges)
library(viridis)
library(formula.tools)
library(xgboost)
library(DT)
library(gridExtra)

col_pal <- magma(5)

source("train_xgb.R")
source("create_Z_varest.R")


dci_palette <- function() {
  
  m_blue_grey <- rgb(52, 64, 77, maxColorValue = 255)
  m_blue_green_1 <- rgb(113, 190, 196, maxColorValue = 255)
  m_blue_green_2 <- rgb(94, 126, 144, maxColorValue = 255)
  m_green <- rgb(80, 188, 138, maxColorValue = 255)
  m_ice_blue <- rgb(161, 179, 192, maxColorValue = 255)
  m_red <- rgb(239, 95, 91, maxColorValue = 255)
  m_black <- rgb(0, 0, 0, maxColorValue = 255)
  
  return(c(m_blue_grey,
           m_blue_green_1,
           m_blue_green_2,
           m_green,
           m_ice_blue,
           m_red,
           m_black))
}