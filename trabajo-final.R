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
#library(leaps)
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
setDT(melt(N))[Var1 != Var2, .SD[which.max(abs(value))], keyby=Var1]

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

pca$rotation
# Mg y Ca tienen coeficientes más altos 


biplot(x = pca, scale = 0, cex = 0.6)
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
indexes <- kfold(X, k = kfold_val)

for(i in 1:kfold_val) {
  kfold_data <- data
  testing_set <- data[FALSE,]
  for(j in 1:length(indexes)) {
    if(i == indexes[j]) {
      testing_set <- rbind(testing_set, data[j,])
      kfold_data <- kfold_data[-j,]
    }
  }
  
  kfold_X <- kfold_data[,1:(ncol(data)-1)]
  ## Seleccion de modelo ## 
  #subset_training_data <- training_data
  #subset_training_data$isadult <- NULL
  #fit_all = regsubsets(training_data$isadult ~ ., subset_training_data)
  #summary(fit_all)
  kfold_data$type_1 <- relevel(kfold_data$type, ref = "1")
  test <- multinom(type_1 ~ ., data = kfold_data)
  summary(test)
  fitted(test)

}



# Linear discriminant analysis #
# Quadratic discriminant analysis #
# Decision trees #
# Naive Bayes? #
# NN? #
# KNN? #
# No parametrico? #
