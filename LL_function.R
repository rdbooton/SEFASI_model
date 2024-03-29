


LL_function  <- function(DATA_INPUT,input_country){
  
  res_H <- res.table[res.table$country==input_country & res.table$var=="H",]
  res_A <- res.table[res.table$country==input_country & res.table$var=="A",]
  res_E <- res.table[res.table$country==input_country & res.table$var=="E",]
  
  LL_H <- rep(NA,nrow(res_H))
  LL_A <- rep(NA,nrow(res_A))
  LL_E <- rep(NA,nrow(res_E))
  
  times_H <- res_H$time
  times_A <- res_A$time
  times_E <- res_E$time
  
  for (i in 1:length(LL_H)) {
    LL_H[i] = res_H$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H[i],".H",sep="")))) + (1- res_H$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H[i],".H",sep="")))) +  
      log(chooseZ(1,res_H$percent[i]))
  }
  
  for (i in 1:length(LL_A)) {
    LL_A[i] = res_A$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A[i],".A",sep="")))) + (1- res_A$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A[i],".A",sep="")))) +  
      log(chooseZ(1,res_A$percent[i]))
  }
  
  for (i in 1:length(LL_E)) {
    LL_E[i] = res_E$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E[i],".E",sep="")))) + (1- res_E$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E[i],".E",sep="")))) +  
      log(chooseZ(1,res_E$percent[i]))
  }
  LL_H_weight <- sum(LL_H)/length(LL_H)
  LL_A_weight <- sum(LL_A)/length(LL_A)
  LL_E_weight  <- sum(LL_E)/length(LL_E)
  toReturn <- as.vector(c(LL_H,LL_A,LL_E,LL_H_weight,LL_A_weight,LL_E_weight))

 names(toReturn) <-  
         c(paste0("LL_H", 1: length(LL_H)),
           paste0("LL_A", 1: length(LL_A)),
           paste0("LL_E", 1: length(LL_E)),
           "LL_H_weight","LL_A_weight","LL_E_weight"
           )
 
 
 return(toReturn)
         
}