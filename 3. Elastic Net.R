# Elastic net
library(glmnet)

#fit model on training set
e_net_default =  glmnet( x = mm[folds < 9,], 
                            y = response[folds < 9], 
                            family="binomial")
                           
 
 
#make predictions on test set 
p_e_net_default =   predict(e_net_default, 
                        newx = mm[folds >= 9,], 
                        type="response")
 
str(p_e_net_default )

 # num [1:98, 1:100] 0.467 0.467 0.467 0.467 0.467 ...
 # - attr(*, "dimnames")=List of 2
  # ..$ : chr [1:98] "11" "12" "15" "17" ...
  # ..$ : chr [1:100] "s0" "s1" "s2" "s3" ...

#get auc for each prediction
e_net_default_auc = aaply(p_e_net_default, 2, function(.x){
    auc(predicted_prob =  .x, actual_class =  response[ folds >= 9])
    })
    
#plot these results
par(mfrow = c(1,2))
plot(e_net_default, xvar = "lambda")
plot(y = e_net_default_auc , x = log(e_net_default$ lambda ), xlab = 'Log Lambda', ylab = 'AUC', type = 'l', lwd = 2)


#put results in a single dataframe
results$ e_net_default_prob = p_e_net_default[,which.max(e_net_default_auc)]
results$e_net_default_class = p_e_net_default[,which.max(e_net_default_auc)] > 0.5
    
auc(results$e_net_default_prob, results$actual )
accuracy(results$e_net_default_prob > 0.5, results$actual )

#other things:  cross validation of lambda (and alpha) using cv.glmnet
#More data transofrmations, interactions
 