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
library(kableExtra)

col_pal <- magma(5)

source("train_xgb.R")
source("create_Z_varest.R")

# Daimler colors
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

# custom function to plot lime objects
plot_lime <- function(lime_object, model_type){
  title_string <- plot(lime_object)$labels$title
  
  lime_object$results %>%
    ggplot() +
    geom_bar(aes(x = reorder(feature.value, effect),
                 y = effect),
             stat = "identity",
             fill = "#5E7E90",
             alpha = 0.95) +
    coord_flip() +
    theme_minimal() +
    labs(x = "",
         y = "Feature Attribution",
         title = title_string,
         subtitle = model_type)
}

# custom function to plot breakdwon plots

plot_waterfall <- function(explain_object, title_string){
  
  df_explained <- as.data.frame(explain_object) 
  
  intercept <- df_explained %>%
    dplyr::filter(variable == "intercept") %>%
    dplyr::pull(contribution)
  
  prediction <- df_explained %>%
    dplyr::filter(variable == "prediction") %>%
    dplyr::pull(contribution)
  
  df_explained <- df_explained %>%
    dplyr::select(-label) %>%
    dplyr::select(variable, contribution, end = cummulative, value = variable_value, sign, position) %>% 
    dplyr::mutate(start = dplyr::lag(end),
                  sign_new = case_when(sign == "1" ~ "+",
                                       sign == "0" ~ "",
                                       sign == "-1" ~ "-",
                                       TRUE ~ "+"),
                  value = as.numeric(as.character(value)),
                  label = paste(gsub(pattern = " =.*", replacement = "", x = variable),
                                "=", 
                                round(value)),
                  label = ifelse(variable == "prediction", "prediction", label),
                  label = ifelse(variable == "intercept", "intercept", label)) %>%
    dplyr::mutate(start = ifelse(variable == "intercept", intercept, start)) %>% 
    dplyr::arrange(position)
  
  df_explained %>% 
    ggplot(., aes(x = reorder(label, position)),
           alpha = 0.9) + 
    geom_segment(aes(xend = label,
                     y = ifelse(label == "intercept", end, start),
                     yend = end,
                     colour = sign_new),
                 size = 7) +
    geom_segment(data = dplyr::filter(df_explained, variable == "prediction"),
                 aes(xend = label, 
                     y = end, 
                     yend = intercept), 
                 size = 7,
                 color = "#34404D") +
    geom_text(aes(x = label, 
                  y = ifelse(sign_new == "-", start, end), 
                  label = round(contribution, 3),
                  fontface = ifelse(variable == "prediction", 2, 1)), 
              size = 18,
              nudge_y = mean(abs(df_explained$contribution))/8) +
    geom_hline(aes(yintercept = intercept),
               lty = "dashed",
               alpha = 0.5) +
    scale_color_manual("",
                       values = c("-" = "firebrick4",
                                  "+" = "#50BC8A",
                                  "0" = "black"),
                       labels = c("-" = "Decreasing Prediction",
                                  "+" = "Increasing Prediction",
                                  "0" = "Neutral")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none") +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    labs(x = "", y = "", subtitle = title_string)
}



