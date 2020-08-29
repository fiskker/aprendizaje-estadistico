library(dplyr)

draw_boxplot <- function(data, dep_var, indep_var, dep_var_legend) {
  colorder <- c( "green", "orange", "red", "skyblue", "purple")
  ggplot(data, aes(indep_var, dep_var)
         ) + 
    xlab("Type") + ylab(dep_var_legend) +
    #geom_boxplot(outlier.colour="red", outlier.shape=8,
    #             outlier.size=4, aes(fill=indep_var)) 
    geom_boxplot(aes(fill=as.factor(indep_var))) + 
    geom_point(aes(colour=as.factor(indep_var))) + 
    scale_color_manual(breaks=colorder, # color scale (for points)
                       values=colorder,
                       labels=c("1","2","3","5", "7"),
                       name="Type") +
    scale_fill_manual(values=colorder,
                      labels=c("1","2","3","5", "7"),
                      name="Type") +
    scale_x_discrete(labels=c("1","2","3","5", "7")) +
    theme_bw()
}

get_outlier_indexes <- function(data) {
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
  outlier_indexes
}

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