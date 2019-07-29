train_xgb <- function(form, .data, ...) {
  
  # Target and features
  target <- formula.tools::lhs(form)
  features <- labels(terms(form))
  
  # Separate X/y
  y <- .data %>% dplyr::select(!!target)
  X <- .data %>% dplyr::select(!!features)
  
  # Create final matrices needed for xgBoost:
  train_matrix <- xgb.DMatrix(data = as.matrix(X), 
                              label = as.matrix(y))
  
  # Parameters
  params = list(
    "booster" = "gbtree",
    "objective" = "reg:linear",
    "eval_metric" = "mae"
  )
  
  # Fit model
  mod <- xgboost::xgb.train(params = params,
                            data = train_matrix,
                            nround = 100)

  # Return fitted obj
  return(mod)
}


predict_xgb <- function(model, newdata) {
  
  # Only select features which are relevant for estimation
  X <- .data_new %>%
    ungroup() %>%   
    #na.omit() %>%
    select(!!features) 
  
  
  y <- X %>% select(savings_perc)
  X <- X %>% select(-savings_perc)
  
  # Create matrix required by xgboost
  data_new_matrix <- xgb.DMatrix(data = as.matrix(X), label = as.matrix(y))
  
  pred <- predict(model, data_new_matrix)
  return(pred)
}