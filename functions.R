model_selection <- function(resu){
log_lik <- lapply(resu, logLik)
AIC_table <- AICctab(log_lik, mnames = names(resu), nobs = resu[[1]]$dims$N, weights = TRUE)
resu <- resu[attr(AIC_table, "row.names")]
S.tot <- sapply(resu, function(i) intervals(i)$coef["S.tot",])
Average_S.tot <- sum(S.tot[2, ] * AIC_table$weight[1:length(resu)])
sdt_model <- sapply(resu, function(i) sqrt(i$varBeta["S.tot", "S.tot"] + (i$coefficients["S.tot"]-Average_S.tot)^2))
average_sdte <- sum(sdt_model * AIC_table$weight[1:length(resu)])
upper <- Average_S.tot + 1.96 * average_sdte
lower <- Average_S.tot - 1.96 * average_sdte

return(list(AIC = AIC_table, Average_S_tot = c(lower, Average_S.tot, upper), S.tot = S.tot))
}

prediction <- function(resu, AIC_table){
resu <- resu[attr(AIC_table, "row.names")]  
predictions <- sapply(resu, predict)
average_model <- rowSums(sapply(1:ncol(predictions), function(i) predictions[, i] * AIC_table[["weight"]][i]))
predictions_final <- cbind(predictions, average_model)
return(predictions_final)
}

