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