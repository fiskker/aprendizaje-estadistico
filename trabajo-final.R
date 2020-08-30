library(ggplot2)
library(GGally)
library(MASS)
library(pROC)
library(dplyr)
library(data.table)
library(dismo)
library(nnet)
library(leaps)
library(broom)
library(caret)
library(Hmisc)
library(effects)
library(class)
functions <- "Documents/facultad/aprendizaje-estadistico/trabajo-final/functions.R"
source(file=functions)
dir <- "Documents/facultad/aprendizaje-estadistico/trabajo-final"
find_all_subsets <- function(N, n, vector, current_subset, collapse = "+") {
  subset_list <- current_subset
  if(n > 0) {
    for (i in 1:length(vector)) {
      subset_list <- c(subset_list, vector[i])
      if (i < length(vector)) {
        found_subset <- find_all_subsets(N, n-1, vector[(i+1):length(vector)], subset_list, collapse)
        if(!is.null(found_subset)) {
          group_list[[ as.character(N) ]]  <<- 
            c(group_list[[ as.character(N) ]], paste(found_subset, collapse = collapse))
        }
        subset_list <- subset_list[-length(subset_list)]
      } else {
        if (length(subset_list) == N) {
          found_subset <- subset_list
          if(!is.null(found_subset)) {
            group_list[[ as.character(N) ]]  <<- 
              c(group_list[[ as.character(N) ]], paste(found_subset, collapse = collapse))
          }
        }
      }
    }
  } else {
    subset_list
  }
}
## -- Data -- ##
path <- "Documents/facultad/aprendizaje-estadistico/trabajo-final/vidrio.txt"
data <- read.csv(file = path, header = FALSE)
names(data) <- c("id", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "type")
# Remove type 4 and 6 samples
data <- filter(data, data$type != 6)
data <- filter(data, data$type != 4)

data$type <- as.factor(data$type)


X <- data[,2:(ncol(data)-1)]
data <- data[,2:(ncol(data))]
## -- #### -- ##

## Data exploration ## 
data_description <- describe(data)

data_description

hist(data$RI, col = 'skyblue3', main = "RI", xlab = "RI values")
hist(data$Na, col = 'skyblue3', main = "Na", xlab = "Na values")
hist(data$Mg, col = 'skyblue3', main = "Mg", xlab = "Mg values")
hist(data$Al, col = 'skyblue3', main = "Al", xlab = "Al values")
hist(data$Si, col = 'skyblue3', main = "Si", xlab = "Si values")
hist(data$K, col = 'skyblue3', main = "K", xlab = "K values")
hist(data$Ca, col = 'skyblue3', main = "Ca", xlab = "Ca values")
hist(data$Ba, col = 'skyblue3', main = "Ba", xlab = "Ba values")
hist(data$Fe, col = 'skyblue3', main = "Fe", xlab = "Fe values")

ggplot(data, aes(type)) + geom_bar(aes(fill = type, alpha = 0.4))

cov_data <- data
cov_data$type <- NULL 
cov_data$id <- NULL
N <- cor(cov_data)
setDT(melt(N))[Var1 != Var2, .SD[which.max(abs(value))], keyby=Var1]

cor_matrix <- cor(cov_data)
cor_matrix


cor_matrix[lower.tri(cor_matrix)] <- NA
heatmap_cor <- reshape2::melt(cor_matrix, na.rm = TRUE)

ggheatmap <- ggplot(data = heatmap_cor, aes(Var2, Var1, fill = value)) + geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 9, hjust = 1)) + coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = format(round(value, 2))), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1,0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
pairs_data <- data[,2:(ncol(data)-1)]
ggpairs(pairs_data, aes(colour = as.factor(data$type), alpha = 0.4))



draw_boxplot(data, data$RI, data$type, 'RI')
draw_boxplot(data, data$Na, data$type, 'Na')
draw_boxplot(data, data$Mg, data$type, 'Mg')
draw_boxplot(data, data$Al, data$type, 'Al')
draw_boxplot(data, data$Si, data$type, 'Si')
draw_boxplot(data, data$K, data$type, 'K')
draw_boxplot(data, data$Ca, data$type, 'Ca')
draw_boxplot(data, data$Ba, data$type, 'Ba')
draw_boxplot(data, data$Fe, data$type, 'Fe')

apply(X = X, MARGIN = 2, FUN = mean)
apply(X = X, MARGIN = 2, FUN = var)
## --------------------------- ##

# Outlier via Tukey method
outlier_indexes <- get_outlier_indexes(data)
values <- hist(as.numeric(outlier_indexes), breaks = nrow(data), col = 'skyblue3', xlab = "Indices de outliers",
               main = "Histograma de outliers en muestras")
#line(x = 1:nrow(data), y = rep(2, nrow(data)))
data_outlier_indexes <- character()
for (idx in outlier_indexes) {
  if (length(which(outlier_indexes == idx)) > 3 & !(idx %in% data_outlier_indexes)) {
    data_outlier_indexes <- c(data_outlier_indexes, idx)
  }
}

data_outlier_indexes

for (outlier_idx in as.numeric(data_outlier_indexes)) {
  X <- X[-outlier_idx,]
  data <- data[-outlier_idx,]
}

ggplot(data, aes(type)) + geom_bar(aes(fill = type, alpha = 0.4))
describe(data)

## -- ##

## -- PCA -- ##
pca <- prcomp(X)
summary(pca)

pca$x
pca$rotation
# Mg y Ca tienen coeficientes más altos 


biplot(x = pca, scale = 0, cex = 0.6)
biplot(x = pca, scale = 0, cex = 0.6)
biplot(pca, choices=c(2,3), scale = 0, cex = 0.6)

# Chequear el angulo entre Ca y Mg 

prop_var <- pca$sdev^2 / sum(pca$sdev^2)

ggplot(data = data.frame(prop_var, pc = 1:9),
       aes(x = pc, y = prop_var)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

prop_var_acum <- cumsum(prop_var)

ggplot(data = data.frame(prop_var_acum, pc = 1:9),
       aes(x = pc, y = prop_var_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

# Multinomial logistic regression #

# N-k fold cv
N = 200
error_prediction_values <- numeric()
error_fit_values <- numeric()
aic_values <- numeric()
accuracy_testing_values <- numeric()
accuracy_testing_values_upper <- numeric()
accuracy_testing_values_lower <- numeric()
k_values <- numeric()
model_values <- character()
n_values <- numeric()
n_params <- numeric()
proposed_models_list_mlr <- list()
model_id_values <- numeric()
error_fit_values_upper <- numeric()
error_fit_values_lower <- numeric()
error_prediction_values_upper <- numeric()
error_prediction_values_lower <- numeric()
varsum <- 1
for (n in 1:N) {
  # k = 5 kfold cv.
  kfold_val <- 5
  pca_X <- as.data.frame(pca$x[,1:5])
  pca_data <- pca_X 
  pca_data$type <- data$type
  indexes <- kfold(X, k = kfold_val)
  # K-fold CV
  par(mfrow=c(1,1))
  for(k in 1:kfold_val) {
    kfold_data <- pca_data
    testing_set <- pca_data[FALSE,]
    for(j in 1:length(indexes)) {
      if(k == indexes[j]) {
        testing_set <- rbind(testing_set, pca_data[j,])
        kfold_data <- kfold_data[-j,]
      }
    }
    
    kfold_X <- kfold_data[,1:(ncol(pca_data)-1)]
    
    dep_variables <- names(kfold_X)
    null_model <- multinom(type ~ 1, data = kfold_data, trace = FALSE)
    saturated_model <- multinom(type ~ ., data = kfold_data, trace = FALSE)
    current_model <- null_model
    current_variables <- character()
    best_models <- character()
    for(i in 1:5) {  
      current_pval <- 1
      best_variable <- 0
      for(variable in dep_variables) {
        variables <- c(current_variables, variable)
        formula <- as.formula(
          paste("type",
                paste(variables, collapse = " + "),
                sep = '~')
        )
        
        proposed_model <- multinom(formula, data = kfold_data, trace = FALSE)
        #summary(proposed_model)
        #prop_model_data <- tidy(proposed_model, conf.int = TRUE, exponentiate = TRUE)
        #if (!all(prop_model_data$p.value > 0.05)) {
        #  print("Algun modelo fallo la Wald test")
        #  next 
        #}
        
        # deviance test 
        deviance_proposed <- deviance(proposed_model) # -2 loglik(pm)
        deviance_current <- deviance(current_model) # -2 loglik(cm)
        
        # delta_d = LR = 2(-loglik(cm) + loglik(pm)) ~ Xdf²
        delta_d <- deviance_current - deviance_proposed 
        df <- ncol(coef(proposed_model)) - ncol(coef(current_model))
        
        p_val <- pchisq(delta_d, df, lower.tail=FALSE)
        if(p_val < current_pval && p_val <= 0.05) {
          current_pval <- p_val
          best_proposed_model <- proposed_model
          best_variable <- variable
        }
      }
      if (ncol(coef(best_proposed_model)) == ncol(coef(current_model))) {
        print("Un mejor modelo no fue hallado")
      } else {
        current_model <- best_proposed_model
        #prop_model_data <- tidy(best_proposed_model, conf.int = TRUE, exponentiate = TRUE)
        current_variables <- c(current_variables, best_variable)  
        dep_variables <- dep_variables[dep_variables != best_variable]
        
        # FIT error #
        fit_sum = 0
        fitted <- best_proposed_model$fitted.values
        for (row in 1:nrow(fitted)) {
          col <- which.is.max(fitted[row,])
          fitted_df <- as.data.frame(fitted)
          fitted[row, col] <- 1 
          fit_sum <- fit_sum + as.numeric(names(fitted_df)[col] != kfold_data$type[row])
        }
        
        # Wilson score confidence interval
        z = 1.96 # 0.95 conf int
        error_fit <- fit_sum / nrow(fitted)
        interval <- z * sqrt( (error_fit * (1 - error_fit)) / nrow(fitted))
        error_fit_upper <- error_fit + interval 
        error_fit_lower <- error_fit - interval 
        
        # Predict in test set
        pred <- predict(best_proposed_model, newdata = testing_set, "probs")
        # Confusion table #
        pred_class <- predict(best_proposed_model, newdata = testing_set, "class")
        conf_matrix <- confusionMatrix(pred_class, testing_set$type)
        
        # TEST error #
        sum = 0
        if (is.null(nrow(pred))) {
          # LOO-CV
          col <- which.is.max(pred)
          pred_df <- as.data.frame(t(pred))
          sum <- sum + as.numeric(names(pred_df)[col] != testing_set$type)
          
          # Wilson score confidence interval
          z = 1.96 # 0.95 conf int
          error_prediction <- sum / 1
          interval <- z * sqrt( (error_prediction * (1 - error_prediction)) / 1)
          error_prediction_upper <- error_prediction + interval 
          error_prediction_lower <- error_prediction - interval 
        } else {
          for (row in 1:nrow(pred)) {
            col <- which.is.max(pred[row,])
            pred_df <- as.data.frame(pred)
            pred[row, col] <- 1 
            sum <- sum + as.numeric(names(pred_df)[col] != testing_set$type[row])
          }
          # Wilson score confidence interval
          z = 1.96 # 0.95 conf int
          error_prediction <- sum / nrow(pred)
          interval <- z * sqrt( (error_prediction * (1 - error_prediction)) / nrow(pred))
          error_prediction_upper <- error_prediction + interval 
          error_prediction_lower <- error_prediction - interval 
        }
        #message("N: ", n)
        n_values <- c(n_values, n)
        #message("K: ", k)
        k_values <- c(k_values, k)
        #message("AIC: ", best_proposed_model$AIC)
        n_params <- c(n_params, i)
        #message("Number of parameters: ", i)
        aic_values <- c(aic_values, best_proposed_model$AIC)
        #message("MSE fit: ", fit_sum/nrow(fitted))
        error_fit_values <- c(error_fit_values, error_fit)
        error_fit_values_upper <- c(error_fit_values_upper, error_fit_upper)
        error_fit_values_lower <- c(error_fit_values_lower, error_fit_lower)
        #message(paste(c("Modelo ", current_variables), collapse=" "))
        model_values <- c(model_values, paste(current_variables, collapse = " "))
        #message("MSE testing: ", sum/nrow(pred))
        error_prediction_values <- c(error_prediction_values, error_prediction)
        error_prediction_values_upper <- c(error_prediction_values_upper, error_prediction_upper)
        error_prediction_values_lower <- c(error_prediction_values_lower, error_prediction_lower)
        #message(paste("Overall accuracy: ", conf_matrix$overall[[1]]))
        accuracy_testing_values <- c(accuracy_testing_values, conf_matrix[["overall"]][["Accuracy"]])
        accuracy_testing_values_upper <- c(accuracy_testing_values_upper, conf_matrix[["overall"]][["AccuracyUpper"]])
        accuracy_testing_values_lower <- c(accuracy_testing_values_lower, conf_matrix[["overall"]][["AccuracyLower"]])
        #message(Id: ", varsum)
        model_id_values <- c(model_id_values, varsum)
        proposed_models_list_mlr[[ as.character(varsum) ]] <- best_proposed_model
        varsum <- varsum + 1
      }
    }
  }
}

results_mlr <- data.frame(N = n_values, K = k_values, n_params = n_params, FIT_ERROR = error_fit_values,
                      FIT_ERROR_UPPER = error_fit_values_upper, FIT_ERROR_LOWER = error_fit_values_lower,
                      AIC = aic_values, PREDICTION_ERROR = error_prediction_values,
                      PREDICTION_ERROR_UPPER = error_prediction_values_upper,
                      PREDICTION_ERROR_LOWER = error_prediction_values_lower,
                      ACCURACY = accuracy_testing_values, ACCURACY_UPPER = accuracy_testing_values_upper,
                      ACCURACY_LOWER = accuracy_testing_values_lower, models = model_values, 
                      model_id = model_id_values, stringsAsFactors = FALSE)

two_group_list <- character()
three_group_list <- character()
four_group_list <- character()
five_group_list <- character()
group_list <- list()
group_list[[ as.character(2) ]] <- two_group_list
group_list[[ as.character(3) ]] <- three_group_list
group_list[[ as.character(4) ]] <- four_group_list
group_list[[ as.character(5) ]] <- five_group_list
vector <- c("PC1", "PC2", "PC3", "PC4", "PC5")
subsets <- list()
dep_variables <- names(kfold_X)
models_list_mlr <- list()

for (n_param in 1:5) {
  model <- dplyr::filter(results_mlr, n_params == n_param)
  for (i in 1:nrow(model)) {
    string_model <- as.character(model[i,]$models)
    sorted_model <- sortby(string_model, " ", dep_variables)
    str_sorted_model <- paste(sorted_model, collapse = " ")
    model[i,]$models <- str_sorted_model
  }
  
  find_all_subsets(n_param, n_param, vector, subsets, " ")
  for(group_sorted_model in group_list[[ as.character(n_param) ]]) {
    model_data <- dplyr::filter(model, models == group_sorted_model)
    if (nrow(model_data) > 0) {
      models_list_mlr[[ group_sorted_model ]] <- model_data
    }
  }
}

FIT_ERROR_MEAN <- numeric()
FIT_ERROR_UPPER_MEAN <- numeric()
FIT_ERROR_LOWER_MEAN <- numeric()
PREDICTION_ERROR_MEAN <- numeric()
PREDICTION_ERROR_LOWER_MEAN <- numeric()
PREDICTION_ERROR_UPPER_MEAN <- numeric()
ACCURACY_MEAN <- numeric()
ACCURACY_UPPER_MEAN <- numeric()
ACCURACY_LOWER_MEAN <- numeric()
AIC <- numeric()
N_PARAMS <- numeric()
MODEL_LEN <- numeric()
model_names <- character()
for (model_name in names(models_list_mlr)) {
  model <- models_list_mlr[[ model_name ]]
  FIT_ERROR_MEAN <- c(FIT_ERROR_MEAN, mean(model$FIT_ERROR))
  FIT_ERROR_UPPER_MEAN <- c(FIT_ERROR_UPPER_MEAN, mean(model$FIT_ERROR_UPPER))
  FIT_ERROR_LOWER_MEAN <- c(FIT_ERROR_LOWER_MEAN, mean(model$FIT_ERROR_LOWER))
  PREDICTION_ERROR_MEAN <- c(PREDICTION_ERROR_MEAN, mean(model$PREDICTION_ERROR))
  PREDICTION_ERROR_LOWER_MEAN <- c(PREDICTION_ERROR_LOWER_MEAN, mean(model$PREDICTION_ERROR_LOWER))
  PREDICTION_ERROR_UPPER_MEAN <- c(PREDICTION_ERROR_UPPER_MEAN, mean(model$PREDICTION_ERROR_UPPER))
  ACCURACY_MEAN <- c(ACCURACY_MEAN, mean(model$ACCURACY))
  ACCURACY_UPPER_MEAN <- c(ACCURACY_UPPER_MEAN, mean(model$ACCURACY_UPPER))
  ACCURACY_LOWER_MEAN <- c(ACCURACY_LOWER_MEAN, mean(model$ACCURACY_LOWER))
  AIC <- c(AIC, mean(model$AIC))
  N_PARAMS <- c(N_PARAMS, model$n_params[1])
  MODEL_LEN <- c(MODEL_LEN, nrow(model))
  model_names <- c(model_names, model_name)
  #models_list[[ model_name ]] <- model
}

average_models_mlr <- data.frame(model_name = model_names, n_params = N_PARAMS, model_length = MODEL_LEN,
                             FIT_ERROR_MEAN = FIT_ERROR_MEAN,
                             FIT_ERROR_UPPER_MEAN = FIT_ERROR_UPPER_MEAN, FIT_ERROR_LOWER_MEAN = FIT_ERROR_LOWER_MEAN,
                             PREDICTION_ERROR_MEAN = PREDICTION_ERROR_MEAN,
                             PREDICTION_ERROR_LOWER_MEAN = PREDICTION_ERROR_LOWER_MEAN,
                             PREDICTION_ERROR_UPPER_MEAN = PREDICTION_ERROR_UPPER_MEAN,
                             ACCURACY_MEAN = ACCURACY_MEAN, ACCURACY_UPPER_MEAN = ACCURACY_UPPER_MEAN,
                             ACCURACY_LOWER_MEAN = ACCURACY_LOWER_MEAN, AIC = AIC, stringsAsFactors = FALSE)
# Only once, for writing # 
#write.csv(average_models_mlr, 'Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-avg-models.csv')
average_models_mlr <- read.csv('Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-avg-models.csv')
# for cv comparison # 

# Only once, for writing # 
#write.csv(cv_five, 'Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-cv-five.csv')
#write.csv(cv_ten, 'Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-cv-ten.csv')
#write.csv(cv_loo, 'Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-cv-loo.csv')

cv_five <- read.csv('Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-cv-five.csv')
cv_ten <- read.csv('Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-cv-ten.csv')
cv_loo <- read.csv('Documents/facultad/aprendizaje-estadistico/trabajo-final/MLR-cv-loo.csv')
cv_five_signif <- dplyr::filter(cv_five, model_length > 500)
cv_ten_signif <- dplyr::filter(cv_ten, model_length > 500)
cv_loo_signif <- dplyr::filter(cv_loo, model_length > 500)

cv_x <- 1:5 
plot(cv_x, cv_five_signif$FIT_ERROR_MEAN, xlab = "Model complexity", ylab = "Fit error mean",
     pch=21, cex=2, col='blue',  main = "Fit misslcassification error")
points(cv_x, cv_ten_signif$FIT_ERROR_MEAN, col = 'red', pch = 22)
points(cv_x, cv_loo_signif$FIT_ERROR_MEAN, col = 'green', pch = 24)
legend("topright", c("k = 5", "k = 10", "LOO"),
  col = c("blue", "red", "green"), pch = c(21, 22, 24))

plot(cv_x, cv_five_signif$PREDICTION_ERROR_MEAN, xlab = "Model complexity", ylab = "Fit error mean",
     pch=21, cex=2, col='blue', main = "Prediction missclassification error")
points(cv_x, cv_ten_signif$PREDICTION_ERROR_MEAN, col = 'red', pch = 22)
points(cv_x, cv_loo_signif$PREDICTION_ERROR_MEAN, col = 'green', pch = 24)
legend("topright", c("k = 5", "k = 10", "LOO"),
       col = c("blue", "red", "green"), pch = c(21, 22, 24))

plot(cv_x, cv_five_signif$ACCURACY_MEAN, xlab = "Model complexity", ylab = "Fit error mean",
     pch=21, cex=2, col='blue',  main = "Accuracy")
points(cv_x, cv_ten_signif$ACCURACY_MEAN, col = 'red', pch = 22)
points(cv_x, cv_loo_signif$ACCURACY_MEAN, col = 'green', pch = 24)
legend("bottomright", c("k = 5", "k = 10", "LOO"),
       col = c("blue", "red", "green"), pch = c(21, 22, 24))

#ggplot(data = average_models, aes(x=n_params, y=FIT_ERROR_MEAN)) + 
#      geom_point(stat="identity")
average_models_signif_mlr <- dplyr::filter(average_models_mlr, model_length > 500)
x <- 1:nrow(average_models_signif_mlr)

plot(x, average_models_signif_mlr$FIT_ERROR_MEAN, xlab = "modelos", ylab = "Fit error mean", main = "Fit error mean",
                            pch=16, cex=2, col='skyblue3', ylim = c(min(average_models_signif_mlr$FIT_ERROR_LOWER_MEAN),max(average_models_signif_mlr$FIT_ERROR_UPPER_MEAN)))

arrows(x0=x, y0 = average_models_signif_mlr$FIT_ERROR_LOWER_MEAN, 
       x1=x, y1 = average_models_signif_mlr$FIT_ERROR_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, average_models_signif_mlr$PREDICTION_ERROR_MEAN, xlab = "modelos", ylab = "Prediction error mean",
     main = "Prediction error mean",
     pch=16, cex=2, col='orange', ylim = c(min(average_models_signif_mlr$PREDICTION_ERROR_LOWER_MEAN),max(average_models_signif_mlr$PREDICTION_ERROR_UPPER_MEAN)))

arrows(x0=x, y0 = average_models_signif_mlr$PREDICTION_ERROR_LOWER_MEAN, 
       x1=x, y1 = average_models_signif_mlr$PREDICTION_ERROR_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, average_models_signif_mlr$ACCURACY_MEAN, main = "Accuracy mean", xlab = "modelos", ylab = "Accuracy mean",
     pch=16, cex=2, col='red', ylim = c(0, 1))

arrows(x0=x, y0 = average_models_signif_mlr$ACCURACY_LOWER_MEAN, 
       x1=x, y1 = average_models_signif_mlr$ACCURACY_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, average_models_signif_mlr$AIC, main = "AIC mean", xlab = "modelos", ylab = "AIC",
     pch=16, cex=2, col='green')

# Ver effect (variacion de variables dependientes, como modifican la probabilidad)
# Para correr una vez y sacar los datos, los ids del proposed_models_list dependen
# de cada corrida del modelo
# model_3 <- proposed_models_list_mlr[[3]]
# summary(model_3)
# tidy(model_3, exponentiate = TRUE, conf.int = TRUE)
# eff <- Effect("PC1", model_3)
# eff <- Effect("PC2", model_3)
# eff <- Effect("PC4", model_3)
# data.frame(eff$model.matrix, eff$prob, eff$lower.prob, eff$upper.prob)
# plot(eff)
#plot(allEffects(model_3))

# model_4 <- proposed_models_list_mlr[[4]]
# summary(model_4)
# tidy(model_4, exponentiate = TRUE, conf.int = TRUE)
# eff <- Effect("PC1", model_4)
# plot(eff)
# eff <- Effect("PC2", model_4)
# plot(eff)
# eff <- Effect("PC3", model_4)
# plot(eff)
# eff <- Effect("PC4", model_4)
# plot(eff)
# data.frame(eff$model.matrix, eff$prob, eff$lower.prob, eff$upper.prob)

# model_5 <- proposed_models_list_mlr[[1108]]
# model_5
# summary(model_5)
# print(tidy(model_5, exponentiate = TRUE, conf.int = TRUE), n = 100)
# eff <- Effect("PC1", model_5)
# plot(eff)
# eff <- Effect("PC2", model_5)
# plot(eff)
# eff <- Effect("PC3", model_5)
# plot(eff)
# eff <- Effect("PC4", model_5)
# plot(eff)
# eff <- Effect("PC5", model_5)
# plot(eff)
# data.frame(eff$model.matrix, eff$prob, eff$lower.prob, eff$upper.prob)

# ------ # 

# Linear discriminant analysis #
par(mfrow=c(1,1))

type1_data <- pca_data[pca_data$type == 1,] 
type2_data <- pca_data[pca_data$type == 2,] 
type3_data <- pca_data[pca_data$type == 3,] 
type5_data <- pca_data[pca_data$type == 5,] 
type7_data <- pca_data[pca_data$type == 7,] 

type_list <- list(type1_data, type2_data, type3_data, type5_data, type7_data)
# type 1 #
for(type_data in type_list) {
  print(paste("Tipo: ", type_data[,ncol(type_data)][1]))
  for (i in 1:(ncol(type_data) - 1)) {
    col_data <- type_data[,i]
    test <- shapiro.test(col_data)
    col_name <- names(type_data)[i]
    if (test$p.value < 0.05) {
      print(paste("PV: ", test$p.value, " ", col_name, ": Data significativamente diferente de distrib. normal"))
    } else {
      print(paste(test$p.value,": No se puede refutar"))
    }
  }
}

normplot(type1_data$PC1, "PC1 QQplot", "red")
normplot(type1_data$PC2, "PC2 QQplot", "red")
normplot(type1_data$PC3, "PC3 QQplot", "red")
normplot(type1_data$PC4, "PC4 QQplot", "red")
normplot(type1_data$PC5, "PC5", "red")

# type 2 #
normplot(type2_data$PC1, "PC1 QQplot", "red")
normplot(type2_data$PC2, "PC2 QQplot", "red")
normplot(type2_data$PC3, "PC3 QQplot", "red")
normplot(type2_data$PC4, "PC4 QQplot", "red")
normplot(type2_data$PC5, "PC5", "red")

# type 3 #
normplot(type3_data$PC1, "PC1 QQplot", "red")
normplot(type3_data$PC2, "PC2 QQplot", "red")
normplot(type3_data$PC3, "PC3 QQplot", "red")
normplot(type3_data$PC4, "PC4 QQplot", "red")
normplot(type3_data$PC5, "PC5", "red")

# type 5 #
normplot(type5_data$PC1, "PC1 QQplot", "red")
normplot(type5_data$PC2, "PC2 QQplot", "red")
normplot(type5_data$PC3, "PC3 QQplot", "red")
normplot(type5_data$PC4, "PC4 QQplot", "red")
normplot(type5_data$PC5, "PC5", "red")

# type 7 #
normplot(type7_data$PC1, "PC1 QQplot", "red")
normplot(type7_data$PC2, "PC2 QQplot", "red")
normplot(type7_data$PC3, "PC3 QQplot", "red")
normplot(type7_data$PC4, "PC4 QQplot", "red")
normplot(type7_data$PC5, "PC5", "red")

## ------------------ ##

## all models ## 
two_group_list <- character()
three_group_list <- character()
four_group_list <- character()
five_group_list <- character()
group_list <- list()
group_list[[ as.character(2) ]] <- two_group_list
group_list[[ as.character(3) ]] <- three_group_list
group_list[[ as.character(4) ]] <- four_group_list
group_list[[ as.character(5) ]] <- five_group_list
find_all_subsets(2, 2, vector, subsets)
find_all_subsets(3, 3, vector, subsets)
find_all_subsets(4, 4, vector, subsets)
find_all_subsets(5, 5, vector, subsets)
## -- ## 

# N-Kfold-CV
N = 200
error_prediction_values <- numeric()
error_prediction_values_upper <- numeric()
error_prediction_values_lower <- numeric()
error_fit_values <- numeric()
error_fit_values_upper <- numeric()
error_fit_values_lower <- numeric()
accuracy_testing_values <- numeric()
accuracy_testing_values_upper <- numeric()
accuracy_testing_values_lower <- numeric()
k_values <- numeric()
model_values <- character()
n_values <- numeric()
n_params <- numeric()
proposed_models_list_lda <- list()
model_id_values <- numeric()
varsum <- 1
for(n in 1:N) {
  # K-fold CV
  kfold_val <- 5
  pca_X <- as.data.frame(pca$x[,1:5])
  pca_data <- pca_X 
  pca_data$type <- data$type
  indexes <- kfold(X, k = kfold_val)
  for(k in 1:kfold_val) {
    kfold_data <- pca_data
    testing_set <- pca_data[FALSE,]
    for(j in 1:length(indexes)) {
      if(k == indexes[j]) {
        testing_set <- rbind(testing_set, pca_data[j,])
        kfold_data <- kfold_data[-j,]
      }
    }
    
    kfold_X <- kfold_data[,1:(ncol(pca_data)-1)]
    for (i in 1:5) {
      models <- group_list[[ as.character(i) ]]
      for(model in models) {
        current_variables <- model
        formula <- as.formula(paste("type ~ ", model))
        proposed_model <- lda(formula, data = kfold_data)
        variables <- unlist(strsplit(model, "\\+"))
        length(variables)
        df_formula <- ""
        for (i in 1:length(variables)) {
          a <- paste(variables[i])
          if(i > 1) {
            df_formula <- paste(df_formula, ",", variables[[i]], "=kfold_data$",variables[[i]], sep = "")
          } else {
            df_formula <- paste(variables[[i]], "=kfold_data$",variables[[i]], sep = "")
          }
        }

        df_fit <- parse(text=paste0("data.frame(", df_formula, ")"))
        df_fit <- eval(df_fit)
        L_fit <- cost_function(proposed_model, df_fit, kfold_data$type, FALSE, FALSE)
      
        # Wilson score confidence interval
        z = 1.96 # 0.95 conf int
        interval <- z * sqrt( (L_fit * (1 - L_fit)) / nrow(df_fit))
        L_fit_upper <- L_fit + interval 
        L_fit_lower <- L_fit - interval 
        
        # Predict in test set
        pred <- predict(object = proposed_model, newdata = testing_set)
        
        # Confusion table #
        pred_class <- predict(object = proposed_model, newdata = testing_set)$class
        conf_matrix <- confusionMatrix(pred_class, testing_set$type)
        
        # TEST MSE #
        df_formula <- ""
        for (i in 1:length(variables)) {
          a <- paste(variables[i])
          if(i > 1) {
            df_formula <- paste(df_formula, ",", variables[[i]], "=testing_set$",variables[[i]], sep = "")
          } else {
            df_formula <- paste(variables[[i]], "=testing_set$",variables[[i]], sep = "")
          }
        }
        df_test <- parse(text=paste0("data.frame(", df_formula, ")"))
        df_test <- eval(df_test)
        L_test <- cost_function(proposed_model, df_test, testing_set$type, FALSE, FALSE)
        
        # Wilson score confidence interval
        z = 1.96 # 0.95 conf int
        interval <- z * sqrt( (L_test * (1 - L_test)) / nrow(df_test))
        L_test_upper <- L_test + interval 
        L_test_lower <- L_test - interval 
        
        #message("N: ", n)
        n_values <- c(n_values, n)
        #message("K: ", k)
        k_values <- c(k_values, k)
        n_params <- c(n_params, i)
        #message("MSE fit: ", L_fit)
        error_fit_values <- c(error_fit_values, L_fit)
        error_fit_values_upper <- c(error_fit_values_upper, L_fit_upper)
        error_fit_values_lower <- c(error_fit_values_lower, L_fit_lower)
        #message(paste(c("Modelo ", current_variables), collapse=" "))
        model_values <- c(model_values, paste(current_variables, collapse = " "))
        #message("MSE testing: ", sum/nrow(pred))
        error_prediction_values <- c(error_prediction_values, L_test)
        error_prediction_values_upper <- c(error_prediction_values_upper, L_test_upper)
        error_prediction_values_lower <- c(error_prediction_values_lower, L_test_lower)
        #message(paste("Overall accuracy: ", conf_matrix$overall[[1]]))
        accuracy_testing_values <- c(accuracy_testing_values, conf_matrix[["overall"]][["Accuracy"]])
        accuracy_testing_values_upper <- c(accuracy_testing_values_upper, conf_matrix[["overall"]][["AccuracyUpper"]])
        accuracy_testing_values_lower <- c(accuracy_testing_values_lower, conf_matrix[["overall"]][["AccuracyLower"]])
        #message(Id: ", varsum)
        model_id_values <- c(model_id_values, varsum)
        
        proposed_models_list_lda[[ as.character(varsum) ]] <- proposed_model
        
        varsum <- varsum + 1
      }
    }
  }
}

results_lda <- data.frame(N = n_values, K = k_values, n_params = n_params, FIT_ERROR = error_fit_values,
                      FIT_ERROR_UPPER = error_fit_values_upper, FIT_ERROR_LOWER = error_fit_values_lower,
                      PREDICTION_ERROR = error_prediction_values,
                      PREDICTION_ERROR_UPPER = error_prediction_values_upper,
                      PREDICTION_ERROR_LOWER = error_prediction_values_lower,
                      ACCURACY = accuracy_testing_values, ACCURACY_UPPER = accuracy_testing_values_upper,
                      ACCURACY_LOWER = accuracy_testing_values_lower, models = model_values, 
                      model_id = model_id_values, stringsAsFactors = FALSE)

dep_variables <- names(kfold_X)
models_list_lda <- list()
for (n_param in 2:5) {
  model <- dplyr::filter(results_lda, n_params == n_param)
  for (i in 1:nrow(model)) {
    string_model <- as.character(model[i,]$models)
    sorted_model <- sortby(string_model, "\\+", dep_variables)
    str_sorted_model <- paste(sorted_model, collapse = "+")
    model[i,]$models <- str_sorted_model
  }
  
  for(group_sorted_model in group_list[[ as.character(n_param) ]]) {
    model_data <- dplyr::filter(model, models == group_sorted_model)
    if (nrow(model_data) > 0) {
      models_list_lda[[ group_sorted_model ]] <- model_data
    }
  }
}

FIT_ERROR_MEAN <- numeric()
FIT_ERROR_UPPER_MEAN <- numeric()
FIT_ERROR_LOWER_MEAN <- numeric()
PREDICTION_ERROR_MEAN <- numeric()
PREDICTION_ERROR_LOWER_MEAN <- numeric()
PREDICTION_ERROR_UPPER_MEAN <- numeric()
ACCURACY_MEAN <- numeric()
ACCURACY_UPPER_MEAN <- numeric()
ACCURACY_LOWER_MEAN <- numeric()
N_PARAMS <- numeric()
MODEL_LEN <- numeric()
model_names <- character()
for (model_name in names(models_list_lda)) {
  model <- models_list_lda[[ model_name ]]
  FIT_ERROR_MEAN <- c(FIT_ERROR_MEAN, mean(model$FIT_ERROR))
  FIT_ERROR_UPPER_MEAN <- c(FIT_ERROR_UPPER_MEAN, mean(model$FIT_ERROR_UPPER))
  FIT_ERROR_LOWER_MEAN <- c(FIT_ERROR_LOWER_MEAN, mean(model$FIT_ERROR_LOWER))
  PREDICTION_ERROR_MEAN <- c(PREDICTION_ERROR_MEAN, mean(model$PREDICTION_ERROR))
  PREDICTION_ERROR_LOWER_MEAN <- c(PREDICTION_ERROR_LOWER_MEAN, mean(model$PREDICTION_ERROR_LOWER))
  PREDICTION_ERROR_UPPER_MEAN <- c(PREDICTION_ERROR_UPPER_MEAN, mean(model$PREDICTION_ERROR_UPPER))
  ACCURACY_MEAN <- c(ACCURACY_MEAN, mean(model$ACCURACY))
  ACCURACY_UPPER_MEAN <- c(ACCURACY_UPPER_MEAN, mean(model$ACCURACY_UPPER))
  ACCURACY_LOWER_MEAN <- c(ACCURACY_LOWER_MEAN, mean(model$ACCURACY_LOWER))
  N_PARAMS <- c(N_PARAMS, model$n_params[1])
  MODEL_LEN <- c(MODEL_LEN, nrow(model))
  model_names <- c(model_names, model_name)
  #models_list[[ model_name ]] <- model
}

average_models_lda <- data.frame(model_name = model_names, n_params = N_PARAMS, model_length = MODEL_LEN,
                             FIT_ERROR_MEAN = FIT_ERROR_MEAN,
                             FIT_ERROR_UPPER_MEAN = FIT_ERROR_UPPER_MEAN, FIT_ERROR_LOWER_MEAN = FIT_ERROR_LOWER_MEAN,
                             PREDICTION_ERROR_MEAN = PREDICTION_ERROR_MEAN,
                             PREDICTION_ERROR_LOWER_MEAN = PREDICTION_ERROR_LOWER_MEAN,
                             PREDICTION_ERROR_UPPER_MEAN = PREDICTION_ERROR_UPPER_MEAN,
                             ACCURACY_MEAN = ACCURACY_MEAN, ACCURACY_UPPER_MEAN = ACCURACY_UPPER_MEAN,
                             ACCURACY_LOWER_MEAN = ACCURACY_LOWER_MEAN, stringsAsFactors = FALSE)


# write lda model only once
#write.csv(average_models_lda, 'Documents/facultad/aprendizaje-estadistico/trabajo-final/lda-avg-models.csv')
average_models_lda <- read.csv('Documents/facultad/aprendizaje-estadistico/trabajo-final/lda-avg-models.csv')

plot(average_models_lda$n_params, average_models_lda$FIT_ERROR_MEAN, xlab = "Model complexity", ylab = "Fit error mean", main = "Fit error mean",
     pch=21, cex=2, col='red', ylim = c(min(average_models_lda$FIT_ERROR_MEAN),max(average_models_lda$FIT_ERROR_MEAN)))

plot(average_models_lda$n_params, average_models_lda$PREDICTION_ERROR_MEAN, xlab = "Model complexity", ylab = "Prediction error mean", main = "Prediction error mean",
     pch=21, cex=2, col='orange', ylim = c(min(average_models_lda$PREDICTION_ERROR_MEAN),max(average_models_lda$PREDICTION_ERROR_MEAN)))

plot(average_models_lda$n_params, average_models_lda$ACCURACY_MEAN, xlab = "Model complexity", ylab = "Accuracy", main = "Accuracy mean",
     pch=21, cex=2, col='darkgreen', ylim = c(min(average_models_lda$ACCURACY_MEAN),max(average_models_lda$ACCURACY_MEAN)))

top_fit_lda <- average_models_lda[with(average_models_lda, order(FIT_ERROR_MEAN)),][1:5,]
top_accuracy_lda <- average_models_lda[with(average_models_lda, order(PREDICTION_ERROR_MEAN)),][1:5,]
top_accuracy_lda <- average_models_lda[with(average_models_lda, order(-ACCURACY_MEAN)),][1:5,]
merge(top_fit_lda, top_accuracy_lda)

merge_df <- rbind(top_fit_lda, top_accuracy_lda, top_accuracy_lda)
merge_df <- unique(merge_df)

average_models_signif_lda <- merge_df[with(merge_df, order(n_params)),]

x <- 1:nrow(merge_df)

plot(x, merge_df$FIT_ERROR_MEAN, xlab = "Modelos", ylab = "Fit error mean", main = "Fit error mean",
     pch=16, cex=2, col='skyblue3', ylim = c(0,0.5))

arrows(x0=x, y0 = merge_df$FIT_ERROR_LOWER_MEAN, 
       x1=x, y1 = merge_df$FIT_ERROR_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, merge_df$PREDICTION_ERROR_MEAN, xlab = "Modelos", ylab = "Prediction error mean",
     main = "Prediction error mean",
     pch=16, cex=2, col='orange', ylim = c(0,0.6))

arrows(x0=x, y0 = merge_df$PREDICTION_ERROR_LOWER_MEAN, 
       x1=x, y1 = merge_df$PREDICTION_ERROR_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, merge_df$ACCURACY_MEAN, main = "Accuracy mean", xlab = "Modelos", ylab = "Accuracy mean",
     pch=16, cex=2, col='red', ylim = c(0, 1))

abline(h = merge_df$ACCURACY_MEAN[1], lty = 2)
arrows(x0=x, y0 = merge_df$ACCURACY_LOWER_MEAN, 
       x1=x, y1 = merge_df$ACCURACY_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

# KNN #
library(class)

# N-Kfold-CV
N = 200
error_prediction_values <- numeric()
error_prediction_values_upper <- numeric()
error_prediction_values_lower <- numeric()
error_fit_values <- numeric()
error_fit_values_upper <- numeric()
error_fit_values_lower <- numeric()
accuracy_testing_values <- numeric()
accuracy_testing_values_upper <- numeric()
accuracy_testing_values_lower <- numeric()
k_values <- numeric()
model_values <- character()
n_values <- numeric()
knn_k_values <- numeric()
knn_k <- c(1, 2, 5, 10, 25, 50, 100)
for(n in 1:N) {
  # K-fold CV
  kfold_val <- 5
  pca_X <- as.data.frame(pca$x[,1:5])
  pca_data <- pca_X 
  pca_data$type <- data$type
  indexes <- kfold(X, k = kfold_val)
  for(k in 1:kfold_val) {
    kfold_data <- pca_data
    testing_set <- pca_data[FALSE,]
    for(j in 1:length(indexes)) {
      if(k == indexes[j]) {
        testing_set <- rbind(testing_set, pca_data[j,])
        kfold_data <- kfold_data[-j,]
      }
    }
    
    kfold_X <- kfold_data[,1:(ncol(pca_data)-1)]
    testing_X <- testing_set[,1:(ncol(pca_data)-1)]
    
    kfold_type1_data <- kfold_data[kfold_data$type == 1,] 
    kfold_type2_data <- kfold_data[kfold_data$type == 2,] 
    kfold_type3_data <- kfold_data[kfold_data$type == 3,] 
    kfold_type5_data <- kfold_data[kfold_data$type == 5,] 
    kfold_type7_data <- kfold_data[kfold_data$type == 7,] 
    
    testing_type1_data <- testing_set[testing_set$type == 1,] 
    testing_type2_data <- testing_set[testing_set$type == 2,] 
    testing_type3_data <- testing_set[testing_set$type == 3,] 
    testing_type5_data <- testing_set[testing_set$type == 5,] 
    testing_type7_data <- testing_set[testing_set$type == 7,] 

    knn_train <- rbind(kfold_type1_data, kfold_type1_data,
                       kfold_type3_data, kfold_type5_data,
                       kfold_type7_data)
    knn_test <- rbind(testing_type1_data, testing_type2_data,
                      testing_type3_data, testing_type5_data,
                      testing_type7_data)
    
    
    for (K in knn_k) {
      fit_model <- knn(knn_train[,1:5], knn_train[,1:5], knn_train$type, k=K)
      test_model <- knn(knn_train[,1:5], knn_test[,1:5], knn_train$type, k=K)
      knn_train[,1:5]
      fit_model
      
      L_fit <- sum(knn_train$type != fit_model)/nrow(knn_train)
      # Wilson score confidence interval
      z = 1.96 # 0.95 conf int
      interval <- z * sqrt( (L_fit * (1 - L_fit)) / nrow(knn_train))
      L_fit_upper <- L_fit + interval 
      L_fit_lower <- L_fit - interval 
      
      L_test <- sum(knn_test$type != test_model)/nrow(knn_test)
      # Wilson score confidence interval
      z = 1.96 # 0.95 conf int
      interval <- z * sqrt( (L_test * (1 - L_test)) / nrow(knn_test))
      L_test_upper <- L_test + interval 
      L_test_lower <- L_test - interval 
      
      conf_matrix <- confusionMatrix(knn_test$type, test_model)
      #message("N: ", n)
      n_values <- c(n_values, n)
      #message("K: ", k)
      k_values <- c(k_values, k)
      #message("MSE fit: ", L_fit)
      knn_k_values <- c(knn_k_values, K)
      #message("knn_k_values: ", K)
      error_fit_values <- c(error_fit_values, L_fit)
      error_fit_values_upper <- c(error_fit_values_upper, L_fit_upper)
      error_fit_values_lower <- c(error_fit_values_lower, L_fit_lower)
      #message(paste(c("Modelo ", current_variables), collapse=" "))
      model_values <- c(model_values, paste(current_variables, collapse = " "))
      #message("MSE testing: ", sum/nrow(pred))
      error_prediction_values <- c(error_prediction_values, L_test)
      error_prediction_values_upper <- c(error_prediction_values_upper, L_test_upper)
      error_prediction_values_lower <- c(error_prediction_values_lower, L_test_lower)
      #message(paste("Overall accuracy: ", conf_matrix$overall[[1]]))
      accuracy_testing_values <- c(accuracy_testing_values, conf_matrix[["overall"]][["Accuracy"]])
      accuracy_testing_values_upper <- c(accuracy_testing_values_upper, conf_matrix[["overall"]][["AccuracyUpper"]])
      accuracy_testing_values_lower <- c(accuracy_testing_values_lower, conf_matrix[["overall"]][["AccuracyLower"]])
      #message(Id: ", varsum)
    }
  }
}

results_knn <- data.frame(N = n_values, K = k_values, knn_K = knn_k_values, FIT_ERROR = error_fit_values,
                          FIT_ERROR_UPPER = error_fit_values_upper, FIT_ERROR_LOWER = error_fit_values_lower,
                          PREDICTION_ERROR = error_prediction_values,
                          PREDICTION_ERROR_UPPER = error_prediction_values_upper,
                          PREDICTION_ERROR_LOWER = error_prediction_values_lower,
                          ACCURACY = accuracy_testing_values, ACCURACY_UPPER = accuracy_testing_values_upper,
                          ACCURACY_LOWER = accuracy_testing_values_lower, stringsAsFactors = FALSE)

FIT_ERROR_MEAN <- numeric()
FIT_ERROR_UPPER_MEAN <- numeric()
FIT_ERROR_LOWER_MEAN <- numeric()
PREDICTION_ERROR_MEAN <- numeric()
PREDICTION_ERROR_LOWER_MEAN <- numeric()
PREDICTION_ERROR_UPPER_MEAN <- numeric()
ACCURACY_MEAN <- numeric()
ACCURACY_UPPER_MEAN <- numeric()
ACCURACY_LOWER_MEAN <- numeric()
K_VALUES <- knn_k
for (i in knn_k) {
  filtered_results <- dplyr::filter(results_knn, knn_K == i)
  
  FIT_ERROR_MEAN <- c(FIT_ERROR_MEAN, mean(filtered_results$FIT_ERROR))
  FIT_ERROR_UPPER_MEAN <- c(FIT_ERROR_UPPER_MEAN, mean(filtered_results$FIT_ERROR_UPPER))
  FIT_ERROR_LOWER_MEAN <- c(FIT_ERROR_LOWER_MEAN, mean(filtered_results$FIT_ERROR_LOWER))
  
  PREDICTION_ERROR_MEAN <- c(PREDICTION_ERROR_MEAN, mean(filtered_results$PREDICTION_ERROR))
  PREDICTION_ERROR_UPPER_MEAN <- c(PREDICTION_ERROR_UPPER_MEAN, mean(filtered_results$PREDICTION_ERROR_UPPER))
  PREDICTION_ERROR_LOWER_MEAN <- c(PREDICTION_ERROR_LOWER_MEAN, mean(filtered_results$PREDICTION_ERROR_LOWER))
  
  ACCURACY_MEAN <- c(ACCURACY_MEAN, mean(filtered_results$ACCURACY))
  ACCURACY_UPPER_MEAN<- c(ACCURACY_UPPER_MEAN, mean(filtered_results$ACCURACY_UPPER))
  ACCURACY_LOWER_MEAN<- c(ACCURACY_LOWER_MEAN, mean(filtered_results$ACCURACY_LOWER))
  
}

average_models_knn <- data.frame(K = K_VALUES, FIT_ERROR_MEAN = FIT_ERROR_MEAN,
                                 FIT_ERROR_UPPER_MEAN = FIT_ERROR_UPPER_MEAN, FIT_ERROR_LOWER_MEAN = FIT_ERROR_LOWER_MEAN,
                                 PREDICTION_ERROR_MEAN = PREDICTION_ERROR_MEAN,
                                 PREDICTION_ERROR_LOWER_MEAN = PREDICTION_ERROR_LOWER_MEAN,
                                 PREDICTION_ERROR_UPPER_MEAN = PREDICTION_ERROR_UPPER_MEAN,
                                 ACCURACY_MEAN = ACCURACY_MEAN, ACCURACY_UPPER_MEAN = ACCURACY_UPPER_MEAN,
                                 ACCURACY_LOWER_MEAN = ACCURACY_LOWER_MEAN, stringsAsFactors = FALSE)

#write.csv(average_models_knn, 'Documents/facultad/aprendizaje-estadistico/trabajo-final/knn-avg-models.csv')
average_models_knn <- read.csv('Documents/facultad/aprendizaje-estadistico/trabajo-final/knn-avg-models.csv')


x <- K_VALUES
plot(x, average_models_knn$FIT_ERROR_MEAN, xlab = "Valor de K", ylab = "Fit error mean", main = "Fit error mean",
     pch=16, cex=2, col='skyblue3', ylim = c(0,0.5))

arrows(x0=x, y0 = average_models_knn$FIT_ERROR_LOWER_MEAN, 
       x1=x, y1 = average_models_knn$FIT_ERROR_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, average_models_knn$PREDICTION_ERROR_MEAN, xlab = "Valor de K", ylab = "Prediction error mean",
     main = "Prediction error mean",
     pch=16, cex=2, col='orange', ylim = c(0,0.8))

arrows(x0=x, y0 = average_models_knn$PREDICTION_ERROR_LOWER_MEAN, 
       x1=x, y1 = average_models_knn$PREDICTION_ERROR_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

plot(x, average_models_knn$ACCURACY_MEAN, main = "Accuracy mean", xlab = "Valor de K", ylab = "Accuracy mean",
     pch=16, cex=2, col='red', ylim = c(0, 1))

abline(h = average_models_knn$ACCURACY_MEAN[1], lty = 2)
arrows(x0=x, y0 = average_models_knn$ACCURACY_LOWER_MEAN, 
       x1=x, y1 = average_models_knn$ACCURACY_UPPER_MEAN,
       code = 3, angle = 90, length = 0.1)

# Decision trees #
library(gbm)
library(rpart.plot) 
library(rpart)
partition <- createDataPartition(y = pca_data$type, p = 0.7, list = FALSE)
tree_train <- pca_data[partition, ]
tree_test <- pca_data[-partition, ]
set.seed(123)
tree_model <- rpart(formula = type ~ .,
                       data = tree_train,
                       method = "class",  
                       xval = 5 # 5-fold cross-validation 
)
rpart.plot(tree_model, yesno = TRUE)
printcp(tree_model)
plotcp(tree_model)

pruned_tree_model <- prune(tree_model, 
                  cp = tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"])
rpart.plot(pruned_tree_model, yesno = TRUE)

tree_pred <- predict(pruned_tree_model, tree_test, type = "class")
plot(tree_test$type, tree_pred, 
     main = "Simple Classification: Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted")

tree_test
tree_conf_matrix <- confusionMatrix(data = tree_pred, 
                                  reference = tree_test$type)
tree_conf_matrix

tree_train
tree_train$type <- make.names(tree_train$type)
random_forest <- train(type ~ ., 
                data = tree_train, 
                method = "ranger", 
                tuneLength = 5,  
                metric = "ROC",  
                trControl = trainControl(
                  method = "cv",  
                  number = 5,  
                  savePredictions = "final",      
                  classProbs = TRUE,  
                )
)

plot(random_forest)

pred_rforest <- predict(random_forest, tree_test, type = "raw")
plot(tree_test$type, pred_rforest, 
     main = "Random Forest Classification: Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted")

ref <- as.factor(make.names(tree_test$type))
ref
pred_rforest
rforest_conf_matrix <- confusionMatrix(data = pred_rforest,
                                       reference = ref)

rforest_conf_matrix


tree_gbm <- train(type ~ ., 
                data = tree_train, 
                method = "gbm",  
                tuneLength = 5,  
                metric = "ROC",  
                trControl = trainControl(
                  method = "cv",  
                  number = 5,  
                  savePredictions = "final",       
                  classProbs = TRUE,  
                )
)

plot(tree_gbm)
gbm_pred <- predict(tree_gbm, tree_test, type = "raw")
gbm_conf_matrix <- confusionMatrix(data = gbm_pred,
                                       reference = ref)
gbm_conf_matrix 

rforest_conf_matrix

rf_results <- c(rforest_conf_matrix[["overall"]][["Accuracy"]],
                   rforest_conf_matrix[["overall"]][["AccuracyUpper"]],
                   rforest_conf_matrix[["overall"]][["AccuracyLower"]])
gbm_results <- c(gbm_conf_matrix[["overall"]][["Accuracy"]],
                    gbm_conf_matrix[["overall"]][["AccuracyUpper"]],
                    gbm_conf_matrix[["overall"]][["AccuracyLower"]])

tree_results <- c(tree_conf_matrix[["overall"]][["Accuracy"]],
                  tree_conf_matrix[["overall"]][["AccuracyUpper"]],
                  tree_conf_matrix[["overall"]][["AccuracyLower"]])
mlr_results <- c(max(average_models_signif_mlr$ACCURACY_MEAN),
                       max(average_models_signif_mlr$ACCURACY_UPPER_MEAN),
                       max(average_models_signif_mlr$ACCURACY_LOWER_MEAN))
lda_results <- c(max(average_models_signif_lda$ACCURACY_MEAN),
                       max(average_models_signif_lda$ACCURACY_UPPER_MEAN),
                       max(average_models_signif_lda$ACCURACY_LOWER_MEAN))
knn_results <- c(max(average_models_knn$ACCURACY_MEAN),
                       max(average_models_knn$ACCURACY_UPPER_MEAN),
                       max(average_models_knn$ACCURACY_LOWER_MEAN))

end_results_table <- data.frame(RF = I(rf_results), GBM = I(gbm_results), TREE = I(tree_results),
                                MLR = I(mlr_results), LDA = I(lda_results), KNN = I(knn_results))

col_names <- names(end_results_table)
col_names
end_results_table[1,]
x <- 1:length(col_names)
plot(x, end_results_table[1,], xaxt = "n", xlab = "Métodos",
     ylab = "Accuracy", col = 'skyblue3', pch = 16, ylim=c(min(end_results_table[3,]),
                               max(end_results_table[2,])))
axis(1, at=x, labels=col_names)
end_results_table[3,]
red2 <- adjustcolor("red", alpha.f=0.4)
arrows(x0=x, y0 = as.numeric(end_results_table[3,]), 
       x1=x, y1 = as.numeric(end_results_table[2,]),
       code = 3, angle = 90, length = 0.1, col = red2)
