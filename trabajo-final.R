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
data$type <- as.factor(data$type)

# Remove type 4 and 6 samples
data <- filter(data, data$type != 6)
data <- filter(data, data$type != 4)
X <- data[,2:(ncol(data)-1)]
data <- data[,2:(ncol(data))]
## -- #### -- ##

## Data exploration ## 
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


apply(X = X, MARGIN = 2, FUN = mean)
apply(X = X, MARGIN = 2, FUN = var)
## --------------------------- ##

# PCA #

# LDA (check normality, best subset?), logistic regression (best subset), trees # 

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

# k = 5 kfold cv.
kfold_val <- 5
pca_X <- as.data.frame(pca$x[,1:5])
pca_data <- pca_X 
pca_data$type <- data$type
indexes <- kfold(X, k = kfold_val)

par(mfrow=c(3,3))
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
  mse_testing <- character()
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
      
      proposed_model <- multinom(formula, data = kfold_data, trace = FALSE)
      tidy(proposed_model)
      
      # deviance test 
      deviance_proposed <- deviance(proposed_model) # -2 log(p(y|tita_0))
      deviance_current <- deviance(current_model) # -2 log(p(y|tita_1))
      
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
      current_variables <- c(current_variables, best_variable)  
      dep_variables <- dep_variables[dep_variables != best_variable]
      
      # FIT MSE #
      fit_sum = 0
      fitted <- best_proposed_model$fitted.values
      for (row in 1:nrow(fitted)) {
        col <- which.is.max(fitted[row,])
        fitted_df <- as.data.frame(fitted)
        fitted[row, col] <- 1 
        fit_sum <- fit_sum + as.numeric(names(fitted_df)[col] != kfold_data$type[row])
      }
      
      # AIC, Loss function for each model. 
      pred <- predict(best_proposed_model, newdata = testing_set, "probs")
      
      sum = 0
      for (row in 1:nrow(pred)) {
        col <- which.is.max(pred[row,])
        pred_df <- as.data.frame(pred)
        pred[row, col] <- 1 
        sum <- sum + as.numeric(names(pred_df)[col] != testing_set$type[row])
      }
      
      
      message("K: ", k)
      message("AIC: ", best_proposed_model$AIC)
      message("MSE fit: ", fit_sum/nrow(fitted))
      message(paste(c("Modelo ", current_variables), collapse=" "))
      message("L: ", sum/nrow(pred))
      mse_testing <- c(mse_testing, sum/nrow(pred))
      varsum <- varsum + 1
    }
  }
  #plot(1:varsum, mse_testing)
  message("--------------")
}


  # Aca ya tenemos los p-valores de la Wald test
# The null hypothesis for all three tests is that the smaller model is the “true” 
# model, a large test statistics indicate that the null hypothesis is false.
# lrt, wald, scores test: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/#:~:text=The%20Wald%20test%20works%20by,are%20simultaneously%20equal%20to%20zero.&text=After%20running%20the%20logistic%20regression,Wald%20test%20can%20be%20used.
# wald test: https://stats.stackexchange.com/questions/60074/wald-test-for-logistic-regression
# general: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# deviance: https://data.princeton.edu/wws509/r/c6s2

# Voy a hacer forward step regression comparando deviances y AIC.


# Linear discriminant analysis #
# Quadratic discriminant analysis #
# Decision trees #
# Naive Bayes? #
# NN? #
# KNN? #
# No parametrico? #
