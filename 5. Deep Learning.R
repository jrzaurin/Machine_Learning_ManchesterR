library(h2o)


# initialise h2o
localH2O <- h2o.init(ip = "localhost")

#get data in h2o format
dat_h2o <- as.h2o(arrhythmia)

#get vector of predictor locations
predictors = which(!names(arrhythmia) %in% c("arrhythmia", "abnormal"  ))
system.time(
#fit model
dl_fit <- h2o.deeplearning(x = predictors, 
                            y = 'abnormal',  
                            training_frame = dat_h2o[which(folds < 9),], 
                            epochs = 50) 
)
#make predictions
pred_dl <- h2o.predict(dl_fit, dat_h2o[which(folds >= 9) , ])

pred_dl <- as.data.frame(pred_dl)

auc( pred_dl$`TRUE.`, response[folds >= 9])
accuracy( pred_dl$`TRUE.`, response[folds >= 9])

results$dl_default_prob = pred_dl$`TRUE.`
results$dl_default_class = pred_dl$`TRUE.` > 0.5

str(results)

results$ensemble_prob = rowMeans(results[ , c('e_net_default_prob', 'boost_default_prob','dl_default_prob' )])

auc( results$ensemble_prob, results$actual)
accuracy( results$ensemble_prob > 0.5, results$actual)



