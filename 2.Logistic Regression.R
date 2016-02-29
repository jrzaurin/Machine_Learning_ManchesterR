
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

arrange(uni_var, p)


fmla2 = formula(paste('abnormal ~', paste(arrange(uni_var, p)  $.id[2:20] , collapse = ' + ')))

#fit a logistic regression
glm1 = glm(fmla2,  data = arrhythmia[folds < 9, , drop = TRUE], family = binomial())

#make predictions
p_logistic = predict(glm1, arrhythmia[folds >= 9, ], type =  "response")

#get predictive accuracy
auc(p_logistic, response[folds >= 9] )
accuracy(p_logistic > 0.5, response[folds >= 9] )