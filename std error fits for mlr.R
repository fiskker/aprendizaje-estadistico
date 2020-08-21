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