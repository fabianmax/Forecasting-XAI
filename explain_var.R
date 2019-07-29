
var_mod_1 <- mod_var$varresult$mercedes
var_mod_2 <- mod_var$varresult$bmw

X_train <- mod_var$datamat
X_plain <- create_Z_varest(mod_var)


var_explainer_1 <- DALEX::explain(model = var_mod_1,
                                  data = X_train,
                                  predict_function = predict.lm)

var_explainer_2 <- DALEX::explain(model = var_mod_2,
                                  data = X_train,
                                  predict_function = predict.lm)


breakdown_plot <- prediction_breakdown(var_explainer_1, 
                                       observation = X_plain)

breakdown_plot <- prediction_breakdown(var_explainer_2, 
                                       observation = X_plain)

plot(breakdown_plot)
