################################  Sampling function #####################################

Sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 16) #from https://www.sciencedirect.com/science/article/pii/S2542519619301305
  
  Samples_SIR[,1] <- 0 + (0.2-0)*Samples_SIR[,1]#LAMBDA_H #vary to max 
  Samples_SIR[,2] <- 0 + (Samples_SIR[,1]-0)*Samples_SIR[,2]   #LAMBDA_A ##vary to max so that A<H
  Samples_SIR[,3] <- 0 + (0.2-0)*Samples_SIR[,3]#no information yet 
  
  Samples_SIR[,4] <- input.table[input.table$parameter=='beta_HH_min',]$value + 
    (1-input.table[input.table$parameter=='beta_HH_min',]$value)*Samples_SIR[,4]  #beta_HH
  Samples_SIR[,5] <- 0 + (1-0)*Samples_SIR[,5] #beta_AA
  Samples_SIR[,7] <- 0 +(Samples_SIR[,4]-0)*Samples_SIR[,7] #beta_AH < beta_HH
  Samples_SIR[,6] <- 0 + (Samples_SIR[,5]-0)*Samples_SIR[,6] #beta_HA < beta_AA
  ###NEED TO CHECK THAT beta_HA is also < beta_AA???
  Samples_SIR[,10] <- 0 + (1-0)*Samples_SIR[,10] #beta_AE 
  Samples_SIR[,8] <- 0 + (1-0)*Samples_SIR[,8]  #beta_HE
  Samples_SIR[,9] <- 0 + (Samples_SIR[,4]-0)*Samples_SIR[,9] ##beta_EH < beta_HH
  Samples_SIR[,11] <-  0 + (1-0)*Samples_SIR[,11]  #beta_EA 
  Samples_SIR[,12] <-  0 + (1-0)*Samples_SIR[,12] #beta_EE
  
  Samples_SIR[,13] <- 0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,13]  #mu_H #Carriage of ESBL or CRE at 12 months  community 25.4% patients 35.2%
  Samples_SIR[,14] <-  0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,14]   #mu_A
  Samples_SIR[,15] <-  0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,15]   #mu_E
  Samples_SIR[,16] <- 0 + (1-0)*Samples_SIR[,16]    #gamma
  #Samples_SIR[,17] <- 0 + (1-0)*Samples_SIR[,17] #epsilon
  
  
  paramsMat_SIR <- data.frame(
    LAMBDA_H=Samples_SIR[,1],LAMBDA_A=Samples_SIR[,2],LAMBDA_E=Samples_SIR[,3],
    beta_HH=Samples_SIR[,4],  beta_AA=Samples_SIR[,5],   beta_HA=Samples_SIR[,6], beta_AH=Samples_SIR[,7],
    beta_HE=Samples_SIR[,8],  beta_EH=Samples_SIR[,9],    beta_AE=Samples_SIR[,10],   beta_EA=Samples_SIR[,11],
    beta_EE = Samples_SIR[,12],
    mu_H = Samples_SIR[,13], mu_A =Samples_SIR[,14], mu_E = Samples_SIR[,15],gamma = Samples_SIR[,16],epsilon=1
  )
  return(paramsMat_SIR)
}
