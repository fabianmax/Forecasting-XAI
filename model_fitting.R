
# Data Loading ------------------------------------------------------------

df <- fread("norway_new_car_sales_by_make.csv")


# Data preparation --------------------------------------------------------

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


# Model fitting -----------------------------------------------------------

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
