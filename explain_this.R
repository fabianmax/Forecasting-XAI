library(data.table)
library(dplyr)
library(lubridate)
library(reshape2)
library(vars)
library(caret)
library(iml)
library(DALEX)
library(breakDown)
library(ggplot2)
library(ggridges)
library(viridis)

col_pal <- magma(5)

# Data loading
df <- fread("~/Desktop/norway_new_car_sales_by_make.csv")

# Data prep
df <- df %>% 
  dplyr::filter(Make == "Mercedes-Benz" | Make == "BMW") %>% 
  dplyr::mutate(Make = case_when(Make == "Mercedes-Benz" ~ "Mercedes",
                                 TRUE ~ Make)) %>% 
  dplyr::mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>% 
  dplyr::select(Date, Quantity, Make) %>% 
  dcast(., Date ~ Make, value.var = "Quantity")

names(df) <- tolower(names(df))

# Feature engineering
df <- df %>% 
  dplyr::mutate(mb_lag_1 = dplyr::lag(mercedes, 1),
                mb_lag_2 = dplyr::lag(mercedes, 2),
                mb_lag_3 = dplyr::lag(mercedes, 3),
                bmw_lag_1 = dplyr::lag(bmw, 1),
                bmw_lag_2 = dplyr::lag(bmw, 2),
                bmw_lag_3 = dplyr::lag(bmw, 3),
                trend = 1:n(),
                seasonal_q1 = ifelse(lubridate::month(date) %in% c(1, 2, 3), 1, 0),
                seasonal_q2 = ifelse(lubridate::month(date) %in% c(4, 5, 6), 1, 0),
                seasonal_q3 = ifelse(lubridate::month(date) %in% c(7, 8, 9), 1, 0),
                seasonal_q4 = ifelse(lubridate::month(date) %in% c(10, 11, 12), 1, 0)) %>% 
  remove_missing(.)

df_train <- df %>% 
  dplyr::filter(date <= ymd("2016-12-01"))

df_test <- df %>% 
  dplyr::filter(date == ymd("2017-01-01"))

form <- mercedes ~ mb_lag_1 + mb_lag_2 + mb_lag_3 + 
  bmw_lag_1 + bmw_lag_2 + bmw_lag_3 + 
  trend + 
  seasonal_q1 + seasonal_q2 + seasonal_q3

mod_lm <- train(form = form,
                data = df_train,
                method = "lm",
                trControl = trainControl(method = "none"))

mod_rf <- train(form = form,
                data = df_train,
                method = "rf",
                trControl = trainControl(method = "none"))

mod_var <- VAR(y = df_train[, c("mercedes", "bmw")], 
               p = 3,
               type = "trend",
               season = 4)


# Explain via IML ---------------------------------------------------------

features <- labels(terms(form))
features <- unlist(strsplit(features, ":"))

X_train <- df_train[, features]
X_test <- df_test[, features]
  
predictor_lm <- Predictor$new(mod_lm, data = X_train, y = df_train$mercedes)

# Feature Imp
imp = FeatureImp$new(predictor_lm, loss = "mae")
imp$results %>% 
  ggplot(., aes(x = reorder(feature, importance), 
                y = importance, ymin = importance.05, ymax = importance.95)) +
    geom_pointrange(color = col_pal[2]) + 
    theme_minimal() + 
    coord_flip() + 
    labs(y = "Importance by Permutation", x = "Feature")

df_imp <- expand.grid(feature = imp$results$feature,
                      id = 1:100,
                      importance = NA)

for (i in as.character(unique(df_imp$feature))) {
  sim <- rnorm(100, 
               imp$results$importance[imp$results$feature == i],
               0.075)
  df_imp$importance[df_imp$feature == i] <- sim
}

df_imp %>% 
  ggplot(., aes(x = importance, y = feature)) + 
  geom_density_ridges(fill = col_pal[4], color = "white") + 
  theme_minimal() + 
  labs(x = "Importance by Permutation", x = "Feature")
  

# Feature interaction
interact = Interaction$new(predictor_lm)
plot(interact)

# ICE
ice = FeatureEffects$new(predictor_lm, features = "trend", method = "ice")
ice$results[[1]][, 1:4] %>% 
  group_by(.id) %>% 
  mutate(level = mean(.y.hat)) %>% 
  ggplot(., aes(x = .feature, y = .y.hat, group = .id, color = level)) + 
    geom_line() + 
    scale_color_viridis(guide = FALSE) + 
    theme_minimal() + 
    labs(x = "Trend", y = "Target")


# LIME
lime.explain = LocalModel$new(predictor_lm, x.interest = X_test)
plot(lime.explain)

# SHAP
shapley <- Shapley$new(predictor_lm, x.interest = X_test)
shapley$plot()


# Explain via Dalex -------------------------------------------------------

explainer <- explain(model = mod_lm, 
                     data = X_train,
                     y = df_train$mercedes,
                     predict_function = function(model, newdata) {
                       predict.train(model, newdata = newdata)
                     })

breakdown_plot <- prediction_breakdown(explainer, observation = X_test)
plot(breakdown_plot)

breakdown_plot %>% 
  dplyr::select(variable, contribution, end = cummulative, sign, position) %>% 
  dplyr::mutate(start = dplyr::lag(end),
                sign_new = case_when(sign == "1" ~ "+",
                                     sign == "X" ~ "+",
                                     sign == "-1" ~ "-",
                                     TRUE ~ "+")) %>% 
  dplyr::mutate(start = ifelse(variable == "(Intercept)", end + 1, start),
                start = ifelse(variable == "final_prognosis", min(end), start)) %>% 
  ggplot(., aes(x = reorder(variable, position))) + 
    geom_segment(aes(xend = variable, y = start, yend = end, colour = sign), size = 20) + 
    geom_text(aes(y = end + 25, label = paste(sign_new, abs(round(contribution, 3))))) +
    scale_color_viridis_d(guide = FALSE) + 
    coord_flip() + 
    theme_minimal() + 
    labs(x = "Feature", y = "Contribution to Forecast")






