train_lgb <- function(form, .data, ...) {
  
  target <- formula.tools::lhs(form)
  features <- labels(terms(form))
  
  # Separate X/y
  y <- .data %>% dplyr::select(!!target)
  X <- .data %>% dplyr::select(!!features)
  
  train_lgb <- lgb.Dataset(data = as.matrix(X), 
                           label = dplyr::pull(y, 1))
  
  # Run training 
  mod <- lgb.train(params = list(num_iterations = 100,
                                 learning_rate = 0.3),
                   data = train_lgb,
                   verbose = 1,
                   obj = "regression",
                   eval = "mean_absolute_error")
  
  # Return model
  return(mod)
  
}

# Function to predict from trained catboost model
predict_lgb <- function(.data_new, features, model) {
  
  .data_new <- separate_X_y(.data_new, features)
  
  # Prediction
  pred <- predict(model, as.matrix(.data_new$X))
  
  # Return prediction
  return(pred)
  
}