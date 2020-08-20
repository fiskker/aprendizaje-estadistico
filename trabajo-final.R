# 2. El archivo Vidrios.txt se tiene datos de 9 variables predictivas para poder clasificar el tipo de vidrio,
# el cual consta de 7 categorı́as. Las categorı́as 4 y 6 no son tenidas en cuenta por falta de datos.
# Detalle:
#  The study of classification of types of glass was motivated by criminological investigation. At the
#scene of the crime, the glass left can be used as evidence...if it is correctly identified!
#  Attribute Information:
#  1. Id number: 1 to 214
#2. RI: refractive index
#3. Na: Sodium (unit measurement: weight percent in corresponding oxide, as are attributes 4-10)
#4. Mg: Magnesium
#5. Al: Aluminum
#6. Si: Silicon
#7. K: Potassium
#8. Ca: Calcium
#9. Ba: Barium
#10. Fe: Iron
#11. Type of glass: (class attribute)
#9. Class Distribution: (out of 214 total instances)

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
#functions <- "Documents/facultad/1c2020/aprendizaje-estadistico/entrega-2/functions.R"
#source(file=functions)

draw_boxplot <- function(data, dep_var, indep_var) {
  ggplot(data, aes(x=indep_var, y=dep_var)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4, aes(fill=indep_var)) 
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

hist(data$RI, col = 'skyblue3', main = "RI", xlab = "RI values")
hist(data$Na, col = 'skyblue3', main = "Na", xlab = "Na values")
hist(data$Mg, col = 'skyblue3', main = "Mg", xlab = "Mg values")
hist(data$Al, col = 'skyblue3', main = "Al", xlab = "Al values")
hist(data$Si, col = 'skyblue3', main = "Si", xlab = "Si values")
hist(data$K, col = 'skyblue3', main = "K", xlab = "K values")
hist(data$Ca, col = 'skyblue3', main = "Ca", xlab = "Ca values")
hist(data$Ba, col = 'skyblue3', main = "Ba", xlab = "Ba values")
hist(data$Fe, col = 'skyblue3', main = "Fe", xlab = "Fe values")

# Outlier via Turkey hunting
outlier_indexes <- character()
for (i in 1:(ncol(data)-1)) {
  Q1 <- quantile(data[,i])[2]
  Q3 <- quantile(data[,i])[4]
  IQR <- Q3 - Q1
  outlier_step <- 1.5 * IQR
  if(outlier_step == 0)
    next
  
  col_name <- names(data)[i]
  min_val <- Q1 - outlier_step
  max_val <- Q3 + outlier_step
  outliers <- dplyr::filter(data, UQ(rlang::sym(col_name)) > UQ(max_val)
                               | UQ(rlang::sym(col_name)) < UQ(min_val))
  
  indexes <- character()
  for (outlier in outliers[,i]) {
    outl_index <- which(data[,i] == outlier)
    if (!(outl_index %in% indexes)) {
      indexes <- c(indexes, which(data[,i] == outlier))
    }
  }
  print(col_name)
  print(indexes)
  outlier_indexes <- c(outlier_indexes, indexes)
}

values <- hist(as.numeric(outlier_indexes), breaks = nrow(data), col = 'skyblue3')
#line(x = 1:nrow(data), y = rep(2, nrow(data)))
data_outlier_indexes <- character()
for (idx in outlier_indexes) {
  if (length(which(outlier_indexes == idx)) > 3 & !(idx %in% data_outlier_indexes)) {
    data_outlier_indexes <- c(data_outlier_indexes, idx)
  }
}

for (outlier_idx in as.numeric(data_outlier_indexes)) {
  X <- X[-outlier_idx,]
  data <- data[-outlier_idx,]
}
## -- ##

# Box cox transformation?
# See correlation with heatmap
# Remove outliers for all the analysis
pairs_data <- data[,2:ncol(data)]
ggpairs(pairs_data, aes(colour = as.factor(data$type), alpha = 0.4))

cov_data<- data
cov_data$type <- NULL 
cov_data$id <- NULL
N <- cov(cov_data)
#setDT(melt(N))[Var1 != Var2, .SD[which.max(abs(value))], keyby=Var1]

cor(cov_data)

draw_boxplot(data, data$RI, data$type)
draw_boxplot(data, data$Na, data$type)
draw_boxplot(data, data$Mg, data$type)
draw_boxplot(data, data$Al, data$type)
draw_boxplot(data, data$Si, data$type)
draw_boxplot(data, data$K, data$type)
draw_boxplot(data, data$Ca, data$type)
draw_boxplot(data, data$Ba, data$type)
draw_boxplot(data, data$Fe, data$type)

ggplot(data, aes(type)) + geom_bar(aes(fill = type, alpha = 0.4))

apply(X = X, MARGIN = 2, FUN = mean)
apply(X = X, MARGIN = 2, FUN = var)
## --------------------------- ##

## -- PCA -- ##
pca <- prcomp(X)
summary(pca)

pca$x
pca$rotation
# Mg y Ca tienen coeficientes más altos 


biplot(x = pca, scale = 0, cex = 0.6)
biplot(x = pca, scale = 0, cex = 0.6)
biplot(pca, choices=c(1,3), scale = 0, cex = 0.6)

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

## -- ##
test_porcentajes <- function(varianzas, q, p0, n) {
  r <- length(varianzas)
  q_prop_var <- varianzas[1:q]
  r_prop_var <- varianzas[(q+1):r]
  a <- sum(q_prop_var)
  b <- sum(r_prop_var)
  a2<-sum(varianzas[1:q]^2)
  b2<-sum(varianzas[(q+1):r]^2)
  
  tita <- (1 - p0)*a - p0*b 
  sigma <- 2*(1 - p0)^2 * a2 + 2*p0^2 * b2
  z <- (sqrt(n)*tita)/sqrt(sigma)
  pv <- 2*(1 - pnorm(abs(z)))
  return(c("p-valor: ", pv, "Z obs.: ", z, tita, sqrt(sigma)))
  
}
varianzas <- pca$sdev^2
varianzas
test <- test_porcentajes(varianzas, 1, 0.95, nrow(X))
test
# Hay algun error con el test, ver si el tema de que la data sea en procentajes
# afecta en algo...

# todo: ICs para los lambdas, chequear igualdad de lambdas.

# Multinomial logistic regression #


# N-k fold cv
N = 10
mse_testing_values <- character()
mse_fit_values <- character()
aic_values <- character()
accuracy_testing_values <- character()
accuracy_testing_values_upper <- character()
accuracy_testing_values_lower <- character()
k_values <- character()
model_values <- character()
n_values <- character()
n_params <- character()
tidy_model_list <- list()
model_id_values <- character()
mse_fit_values_plus <- character()
mse_fit_values_minus <- character()
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
    
    #kfold_data$type_1 <- relevel(kfold_data$type, ref = "1")
    
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
        prop_model_data <- tidy(best_proposed_model, conf.int = TRUE, exponentiate = TRUE)
        current_variables <- c(current_variables, best_variable)  
        dep_variables <- dep_variables[dep_variables != best_variable]
        
        # FIT MSE #
        confint(best_proposed_model)
        summary(best_proposed_model)
        
        #eff <- Effect("PC1", best_proposed_model)
        #data.frame(eff$model.matrix, eff$prob, eff$lower.prob, eff$upper.prob)
        #plot(eff)
        #plot(allEffects(best_proposed_model))
        
        best_proposed_model_smr <- summary(best_proposed_model)
        plus_bpm <- best_proposed_model_smr$coefficients + best_proposed_model_smr$standard.errors
        minus_bpm <- best_proposed_model_smr$coefficients - best_proposed_model_smr$standard.errors
        
        model_matrix <- rep(1, nrow(kfold_X))
        for (variable in current_variables) {
          model_matrix <- cbind(model_matrix, kfold_X[[ variable ]])
        }
        plus_prob <- as.data.frame(exp(t(plus_bpm %*% t(model_matrix))))
        plus_prob <- cbind('1' = rep(0, nrow(plus_prob)), plus_prob)
        for (j in 1:nrow(plus_prob)) {
          factor <- 1 / (1 + sum(plus_prob[j,]))
          plus_prob[j,] <- plus_prob[j,] * factor
          plus_prob[j,]$'1' <- factor
        }
        
        minus_prob <- as.data.frame(exp(t(minus_bpm %*% t(model_matrix))))
        minus_prob <- cbind('1' = rep(0, nrow(minus_prob)), minus_prob)
        for (j in 1:nrow(minus_prob)) {
          factor <- 1 / (1 + sum(minus_prob[j,]))
          minus_prob[j,] <- minus_prob[j,] * factor
          minus_prob[j,]$'1' <- factor
        }
        
        # FIT MSE #
        fit_sum = 0
        fit_sum_plus = 0
        fit_sum_minus = 0
        fitted <- best_proposed_model$fitted.values
        for (row in 1:nrow(fitted)) {
          col <- which.is.max(fitted[row,])
          col_plus <- which.is.max(plus_prob[row,])
          col_minus <- which.is.max(minus_prob[row,])
          fitted_df <- as.data.frame(fitted)
          fitted[row, col] <- 1 
          fit_sum <- fit_sum + as.numeric(names(fitted_df)[col] != kfold_data$type[row])
          fit_sum_plus <- fit_sum_plus + as.numeric(names(plus_prob)[col_plus] != kfold_data$type[row])
          fit_sum_minus <-fit_sum_minus + as.numeric(names(minus_prob)[col_minus] != kfold_data$type[row])
        }
        # Predict in test set
        pred <- predict(best_proposed_model, newdata = testing_set, "probs")
        
        # Confusion table #
        pred_class <- predict(best_proposed_model, newdata = testing_set, "class")
        conf_matrix <- confusionMatrix(pred_class, testing_set$type)
        
        # TEST MSE #
        sum = 0
        for (row in 1:nrow(pred)) {
          col <- which.is.max(pred[row,])
          pred_df <- as.data.frame(pred)
          pred[row, col] <- 1 
          sum <- sum + as.numeric(names(pred_df)[col] != testing_set$type[row])
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
        mse_fit_values <- c(mse_fit_values, fit_sum/nrow(fitted))
        mse_fit_values_plus <- c(mse_fit_values_plus, fit_sum_plus/nrow(plus_prob))
        mse_fit_values_minus <- c(mse_fit_values_minus, fit_sum_minus/nrow(minus_prob))
        
        #message(paste(c("Modelo ", current_variables), collapse=" "))
        model_values <- c(model_values, paste(current_variables, collapse = " "))
        #message("MSE testing: ", sum/nrow(pred))
        mse_testing_values <- c(mse_testing_values, sum/nrow(pred))
        #message(paste("Overall accuracy: ", conf_matrix$overall[[1]]))
        accuracy_testing_values <- c(accuracy_testing_values, conf_matrix[["overall"]][["Accuracy"]])
        accuracy_testing_values_upper <- c(accuracy_testing_values_upper, conf_matrix[["overall"]][["AccuracyUpper"]])
        accuracy_testing_values_lower <- c(accuracy_testing_values_lower, conf_matrix[["overall"]][["AccuracyLower"]])
        #message(Id: ", varsum)
        model_id_values <- c(model_id_values, varsum)
        
        tidy_model_list[[ as.character(varsum) ]] <- prop_model_data
        
        varsum <- varsum + 1
      }
    }
    message("--------------")
  }
}

results <- data.frame(N = n_values, K = k_values, n_params = n_params, MSE_FIT = mse_fit_values,
                      MSE_FIT_UPPER = mse_fit_values_plus, MSE_FIT_LOWER = mse_fit_values_minus,
                      AIC = aic_values, MSE_TESTING = mse_testing_values,
                      accuracy = accuracy_testing_values, accuracy_upper = accuracy_testing_values_upper,
                      accuracy_lower = accuracy_testing_values_lower, models = model_values, 
                      model_id = model_id_values, stringsAsFactors = FALSE)

sortby <- function(string, delimiter, ordering_vector) {
  vector <- strsplit(string, delimiter)
  vector <- vector[[1]]
  if (length(vector) > 1) {
    for (i in 1:length(vector)) {
      for (j in 1:(length(vector) - 1)) {
        left <- match(vector[j], ordering_vector)
        right <- match(vector[j + 1], ordering_vector)
        if (left > right) {
          dummy <- vector[j]
          vector[j] <- vector[j + 1]
          vector[j+1] <- dummy
        }
      }
    }
  }
  vector
}

dep_variables <- names(kfold_X)
models_list <- list()
for (n_param in 1:5) {
  model <- dplyr::filter(results, n_params == n_param)
  for (i in 1:nrow(model)) {
    string_model <- as.character(model[i,]$models)
    sorted_model <- sortby(string_model, " ", dep_variables)
    str_sorted_model <- paste(sorted_model, collapse = " ")
    model[i,]$models <- str_sorted_model
  
    if (!(sorted_model %in% models_list)) {
      model_data <- dplyr::filter(model, models == str_sorted_model)
      models_list[[ str_sorted_model ]] <- model_data
    }
  }
}

mse_fit_mean <- numeric()
mse_test_mean <- numeric()
mse_aic_mean <- numeric()
mse_accuracy_mean <- numeric()
mse_accuracy_upper_mean <- numeric()
mse_accuracy_lower_mean <- numeric()

for (model_name in names(models_list)) {
  model <- models_list[[ model_name ]]
  model$MSE_FIT_MEAN <-  mean(as.numeric(model$MSE_FIT))
  model$MSE_TEST_MEAN <- mean(as.numeric(model$MSE_TESTING))
  model$AIC_MEAN <- mean(as.numeric(model$AIC))
  model$ACCURACY_MEAN <- mean(as.numeric(model$accuracy))
  model$ACCURACY_UPPER_MEAN <- mean(as.numeric(model$accuracy_upper))
  model$ACCURACY_LOWER_MEAN <- mean(as.numeric(model$accuracy_lower))
  mse_fit_mean <- c(mse_fit_mean, mean(as.numeric(model$MSE_FIT)))
  mse_test_mean <- c(mse_test_mean, mean(as.numeric(model$MSE_TESTING)))
  mse_aic_mean <- c(mse_aic_mean, mean(as.numeric(model$AIC)))
  mse_accuracy_mean <- c(mse_accuracy_mean, mean(as.numeric(model$accuracy)))
  mse_accuracy_upper_mean <- c(mse_accuracy_upper_mean, mean(as.numeric(model$accuracy_upper)))
  mse_accuracy_lower_mean <- c(mse_accuracy_lower_mean, mean(as.numeric(model$accuracy_lower)))
  models_list[[ model_name ]] <- model
}

x <- 1:length(models_list)
plot(x, mse_fit_mean, xlab = "modelos", ylab = "MSE fit",
                            pch=16, cex=2, col='skyblue3')

plot(x, mse_test_mean, xlab = "modelos", ylab = "MSE test",
     pch=16, cex=2, col='orange')

plot(x, mse_accuracy_mean, xlab = "modelos", ylab = "Accuracy",
     pch=16, cex=2, col='red', ylim = c(0, 1))

arrows(x0=x, y0 = mse_accuracy_lower_mean, 
       x1=x, y1 = mse_accuracy_upper_mean,
       code = 3, angle = 90, length = 0.1)

plot(x, mse_aic_mean, xlab = "modelos", ylab = "AIC",
     pch=16, cex=2, col='green')

# Aca ya tenemos los p-valores de la Wald test
# The null hypothesis for all three tests is that the smaller model is the “true” 
# model, a large test statistics indicate that the null hypothesis is false.
# lrt, wald, scores test: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/#:~:text=The%20Wald%20test%20works%20by,are%20simultaneously%20equal%20to%20zero.&text=After%20running%20the%20logistic%20regression,Wald%20test%20can%20be%20used.
# wald test: https://stats.stackexchange.com/questions/60074/wald-test-for-logistic-regression
# general: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# deviance: https://data.princeton.edu/wws509/r/c6s2
# Voy a hacer forward step regression comparando deviances.

# ------ # 

# Linear discriminant analysis #
normplot <- function(data, title, line_color) {
  qqnorm(data, main = title, xlab = "Cuantiles normales", ylab = "Cuantiles muestrales")
  qqline(data, col = line_color)
}

detailed_normplot <- function(data, title, line_color, bp_horizontal, hist_title, hist_xlab) {
  normplot(data, title, line_color)
  boxplot(data, horizontal = bp_horizontal)
  hist(data, main = hist_title, xlab = hist_xlab, ylab = "Frecuencia")
}

detailed_normplot_cat <- function(data, category, title, line_color, bp_horizontal, hist_title, hist_xlab) {
  normplot(data, title, line_color)
  barplot(category$Freq, names.arg = category$categorical_data, main = hist_title, xlab = hist_xlab, ylab = "Frecuencia")
}

get_threshold <- function(expected, prediction) {
  my_roc <- multiclass.roc(expected, prediction, levels=base::levels(as.factor(expected)))
  #plot(my_roc)
  coords(my_roc, "best", ret = "threshold", transpose = TRUE)
}

# Cost function
cost_function <- function(fit_function, data, expected, use_roc = TRUE, response = TRUE) {
  n <- length(expected)
  if(response) {
    fit <- predict(fit_function, data, type = "response")
  } else {
    fit <- predict(object = fit_function, newdata = data)  
  }
  
  indicator_vector <- c(rep(1, n))
  if(use_roc) {
    if(response) {
      threshold <- get_threshold(expected, fit)
      H <- fit > threshold
    } else {
      threshold <- get_threshold(expected, fit$posterior[,2])
      H <- fit$posterior[,2] > threshold
    }
  } else {
    threshold <- 0.5
    H <- fit$class
  }
  
  (indicator_vector %*% (H != expected)) / n
}


## QQplots adult data ## 
par(mfrow=c(1,1))

type1_data <- pca_data[pca_data$type == 1,] 
type2_data <- pca_data[pca_data$type == 2,] 
type3_data <- pca_data[pca_data$type == 3,] 
type5_data <- pca_data[pca_data$type == 5,] 
type7_data <- pca_data[pca_data$type == 7,] 

# type 1 #
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


mse_testing_values_list = character()
mse_fit_values_list <- character()
accuracy_testing_values_list <- character()
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
  
  #kfold_data$type_1 <- relevel(kfold_data$type, ref = "1")
  
  dep_variables <- names(kfold_X)
  null_model <- lda(type ~ PC1, data = kfold_data)
  null_model
  current_model <- null_model
  current_variables <- character()
  best_models <- character()
  mse_testing_values <- character()
  mse_fit_values <- character()
  accuracy_testing_values <- character()
  varsum <- 0
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
      
      proposed_model <- lda(type ~ PC1 + PC2 + PC3 + PC4 + PC5, data = kfold_data)
      best_proposed_model <- proposed_model
      current_model <- best_proposed_model
      print(best_proposed_model)
      current_variables <- c(current_variables, best_variable)  
      dep_variables <- dep_variables[dep_variables != best_variable]
      
      
      # FIT MSE #
      df_fit <- data.frame(PC1 = kfold_data$PC1, PC2 = kfold_data$PC2, 
                           PC3 = kfold_data$PC3, PC4 = kfold_data$PC4, PC5 = kfold_data$PC5)
      L_fit <- cost_function(best_proposed_model, df_fit, kfold_data$type, FALSE, FALSE)
      
      # Predict in test set
      pred <- predict(object = best_proposed_model, newdata = testing_set)
      
      # Confusion table #
      pred_class <- predict(object = best_proposed_model, newdata = testing_set)$class
      conf_matrix <- confusionMatrix(pred_class, testing_set$type)

      # TEST MSE #
      df_test <- data.frame(PC1 = testing_set$PC1, PC2 = testing_set$PC2, 
                            PC3 = testing_set$PC3, PC4 = testing_set$PC4, PC5 = testing_set$PC5)
      L_test <- cost_function(best_proposed_model, df_test, testing_set$type, FALSE, FALSE)
      
      message("K: ", k)
      message("MSE fit: ", L_fit)
      mse_fit_values <- c(mse_fit_values, L_fit)
      message(paste(c("Modelo ", current_variables), collapse=" "))
      message("MSE testing: ", L_test)
      mse_testing_values <- c(mse_testing_values, L_test)
      message(paste("Overall accuracy: ", conf_matrix$overall[[1]]))
      accuracy_testing_values <- c(accuracy_testing_values, conf_matrix$overall[[1]])
      
      varsum <- varsum + 1
    }
  }
  mse_testing_values_list <- c(mse_testing_values_list, mse_testing_values)
  mse_fit_values_list <- c(mse_fit_values_list, aic_values)
  accuracy_testing_values_list <- c(accuracy_testing_values_list, accuracy_testing_values)
  
  message("--------------")
}

# lda analisis # 
#type1_data$type <- NULL
#type2_data$type <- NULL
#type3_data$type <- NULL

#z_type1 <- t(best_proposed_model$scaling)%*%t(as.matrix(type1_data))
#z_type2 <- t(best_proposed_model$scaling)%*%t(as.matrix(type2_data))
#z_type3 <- t(best_proposed_model$scaling)%*%t(as.matrix(type3_data))

#plot(z_type1[1,], z_type1[2,], pch=16, col="red", xlab = "LD1", ylab = "LD2")
#points(z_type2[1,], z_type2[2,], pch=16, col="blue")
#points(z_type3[1,], z_type3[2,], pch=16, col="green")








# Quadratic discriminant analysis #
# Decision trees #
# Naive Bayes? #
# NN? #
# KNN? #
# No parametrico? #
