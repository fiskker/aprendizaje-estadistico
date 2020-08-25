type1_data$type <- NULL
type2_data$type <- NULL
type3_data$type <- NULL
left_type1_data <- type1_data[,1:2]
right_type1_data <- type1_data[,5]
right_type1_data <- as.data.frame(right_type1_data)
names(right_type1_data) <- c("PC5")
type1_data <- cbind(left_type1_data, right_type1_data)

left_type2_data <- type2_data[,1:2]
right_type2_data <- type2_data[,5]
right_type2_data <- as.data.frame(right_type2_data)
names(right_type2_data) <- c("PC5")
type2_data <- cbind(left_type2_data, right_type2_data)

left_type3_data <- type3_data[,1:2]
right_type3_data <- type3_data[,5]
right_type3_data <- as.data.frame(right_type3_data)
names(right_type3_data) <- c("PC5")
type3_data <- cbind(left_type3_data, right_type3_data)

models_list_lda[["PC1+PC2+PC5"]][["model_id"]][587]
best_proposed_model <- proposed_models_list[models_list_lda[["PC1+PC2+PC5"]][["model_id"]][587]]$'15249'
z_type1 <- t(best_proposed_model$scaling)%*%t(as.matrix(type1_data))
z_type2 <- t(best_proposed_model$scaling)%*%t(as.matrix(type2_data))
z_type3 <- t(best_proposed_model$scaling)%*%t(as.matrix(type3_data))

plot(z_type1[1,], z_type1[2,], pch=16, col="red", xlab = "LD1", ylab = "LD2")
points(z_type2[1,], z_type2[2,], pch=16, col="blue")
points(z_type3[1,], z_type3[2,], pch=16, col="green")