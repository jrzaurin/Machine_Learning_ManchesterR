library(plyr)
library(dplyr)
#loop through each of the columns in the arrhythmia dataset and check correlation with abnormality
uni_var = ldply(arrhythmia, function(.x){
    p = 1
    if(is.numeric(.x)){
        p = wilcox.test(.x ~ arrhythmia$abnormal) $p.value
        }
        
    if(is.factor(.x)){
        p = chisq.test(x = .x , y = arrhythmia$abnormal) $p.value
        }
        
   data.frame(p=p)
        })

#arrange these by the p value (lowest means more significnat -> more correlation
uni_var = arrange(uni_var, p)

# select the top 20 variables (excluding the first which is the response) and make into a formula
fmla2 = formula(paste('abnormal ~', paste(uni_var$.id[2:21] , collapse = ' + ')))

#fit a logistic regression to this data
glm1 = glm(fmla2,  data = arrhythmia[folds < 9, , drop = TRUE], family = binomial())

#make predictions
p_logistic = predict(glm1, arrhythmia[folds >= 9, ], type =  "response")

results = data.frame(actual =  response[ folds >= 9], logistic_prob = p_logistic )

#get predictive accuracy
auc(p_logistic, response[folds >= 9] )
