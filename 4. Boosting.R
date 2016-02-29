# Boosting
library(xgboost)

#fits model
boost_default = xgboost(data = mm[folds < 9,], 
                            label = response[folds < 9], nrounds = 50, objective = "binary:logistic")
 

#makes predictions
p_boost_default = predict(boost_default, mm[folds >= 9,]) 

auc (p_boost_default, response[folds >= 9] )            
accuracy (p_boost_default > 0.5, response[folds >= 9] )   

#other things:  (cross) validation of eta, nrounds, max.depth

