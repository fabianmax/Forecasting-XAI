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
  return(list(mod = mod,
              target = target,
              features = features))
}


predict_xgb <- function(model, newdata) {

  # Feature matrix
  X_new <- newdata %>% dplyr::select(!!model$features)
  
  # Create matrix required by xgboost
  data_new_matrix <- xgb.DMatrix(data = as.matrix(X_new))
  
  pred <- predict(model$mod, data_new_matrix)
  
  return(pred)
}

