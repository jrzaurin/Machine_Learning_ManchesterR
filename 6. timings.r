# Timings:
library(microbenchmark)

#takes a minute to run...
ml_times = microbenchmark( 
    logistic = glm(fmla2,  data = arrhythmia[folds < 9, , drop = TRUE], family = binomial()),
    en =  glmnet( x = mm[folds < 9,], y = response[folds < 9], family="binomial"),
    boost = xgboost(data = mm[folds < 9,], label = response[folds < 9], nrounds = 50, objective = "binary:logistic"),
    dl = h2o.deeplearning(x = predictors,  y = 'abnormal',  training_frame = dat_h2o[which(folds < 9),],  epochs = 50) ,
     times = 1
    )
    
ml_times = print(ml_times)
str(ml_times)

# accuracy

ml_times$auc = c(70, 80,83,84)
tomr::mp()
plot(auc ~ I(median/1000), ml_times, xlab = 'time to fit (Seconds)')


