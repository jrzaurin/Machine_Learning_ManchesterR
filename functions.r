auc = function (predicted_prob, actual_class) {
    if(!is.logical(actual_class)) actual_class =  as.logical(as.factor(actual_class))
    if(length(actual_class) != length(predicted_prob)) stop('vector lengths do not match')
    rprob = rank(predicted_prob)
    n1 = sum(actual_class)
    n0 = length(actual_class) - n1
    u = sum(rprob[actual_class == 1]) - n1 * (n1 + 1)/2
    u / (n1 * n0)
 }
 
 accuracy  = function(predicted_class, actual_class){
     if(length(predicted_class) != length(predicted_class)) stop('vector lengths do not match')
     
     sum(predicted_class == actual_class) / length(actual_class)
     }