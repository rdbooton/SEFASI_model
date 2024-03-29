
################################  AMRmodel #####################################
#state <- c(H=(1/70000000),A=0,E=0)
state <- c(H=(input.table[input.table$parameter=='H_0',]$value),
           A=(input.table[input.table$parameter=='A_0',]$value),
           E=(input.table[input.table$parameter=='E_0',]$value))
epid.start<- input.table[input.table$parameter=='epid.start',]$value
epid.duration <- input.table[input.table$parameter=='epid.duration',]$value
vectTime <- c(0,1:epid.duration)
int.time <- input.table[input.table$parameter=='int.time',]$value 
eval.time <- input.table[input.table$parameter=='eval.time',]$value 
end.time <- int.time  + eval.time 

#byN=0.1
#beta_HH is the relative attribution of carriage between H and H etc
#gamma is the per-capita rate at which states acquire resistant bacteria as a result of exposure
AMRmodel_DENMARK <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{
    
    if (time <= 2)  {LAMBDA_H <- LAMBDA_H* den_usage[den_usage$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A <- LAMBDA_H/H_A_ratio_den } else if ((time >2) & (time <= 3))  {
        LAMBDA_H <- LAMBDA_H* den_usage[den_usage$year == round((2000+time),0),]$kg/initial_den_H
        LAMBDA_A <- (LAMBDA_H/H_A_ratio_den)*ratio_den_2003_A} else if ((time >3) & (time <= 15))  {  
          LAMBDA_H <- LAMBDA_H* den_usage[den_usage$year == round((2000+time),0),]$kg/initial_den_H
          LAMBDA_A <- (LAMBDA_H/H_A_ratio_den)*ratio_den_2003_A + (time-3)*(((LAMBDA_H/H_A_ratio_den)*ratio_den_2015_A - (LAMBDA_H/H_A_ratio_den)*ratio_den_2003_A)/(15-3)) } else if ((time >15) & (time <= 16))  {  
            LAMBDA_H <- LAMBDA_H* den_usage[den_usage$year == round((2000+time),0),]$kg/initial_den_H
            LAMBDA_A <- (LAMBDA_H/H_A_ratio_den)*ratio_den_2016_A}  else if ((time>16) & (time <= 18))   { 
              LAMBDA_H<- LAMBDA_H* den_usage[den_usage$year == round((2000+time),0),]$kg/initial_den_H
              LAMBDA_A <- (LAMBDA_H/H_A_ratio_den)*ratio_den_2016_A + (time-16)*(((LAMBDA_H/H_A_ratio_den)*ratio_den_2018_A - (LAMBDA_H/H_A_ratio_den)*ratio_den_2016_A)/(18-16))} else if ((time >18) & (time <= 20))  {
                LAMBDA_H <- LAMBDA_H* den_usage[den_usage$year == round((2000+time),0),]$kg/initial_den_H
                LAMBDA_A <-   (LAMBDA_H/H_A_ratio_den)*ratio_den_2018_A + (time-18)*(((LAMBDA_H/H_A_ratio_den)*ratio_den_2020_A - (LAMBDA_H/H_A_ratio_den)*ratio_den_2018_A)/(20-18))  } else if (time >20)  {
                  LAMBDA_H <- LAMBDA_H* den_usage[den_usage$year == (2019),]$kg/initial_den_H 
                  LAMBDA_A <- (LAMBDA_H/H_A_ratio_den)*ratio_den_2021_A }
    
    dH <- gamma*LAMBDA_H*(1-H) + LAMBDA_H*beta_HH*H*(1-H) + LAMBDA_H*beta_AH*(1-H)*A + LAMBDA_H*beta_EH*(1-H)*E - mu_H*H
    dA <-  gamma*LAMBDA_A*(1-A) + LAMBDA_A*beta_AA*A*(1-A) + LAMBDA_A*beta_HA*(1-A)*H + LAMBDA_A*beta_EA*(1-A)*E - mu_A*A
    dE <-  LAMBDA_E*beta_EE*E*(1-E) + LAMBDA_E*beta_HE*(1-E)*H + LAMBDA_E*beta_AE*(1-E)*A - mu_E*E
    # print(c(time,LAMBDA_H,LAMBDA_A,LAMBDA_E))
    return(  list(c(dH,dA,dE)))
})}


AMRmodel_SENEGAL <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{
    
    dH <- gamma*LAMBDA_H*(1-H) + LAMBDA_H*beta_HH*H*(1-H) + LAMBDA_H*beta_AH*(1-H)*A + LAMBDA_H*beta_EH*(1-H)*E - mu_H*H
    dA <-  gamma*LAMBDA_A*(1-A) + LAMBDA_A*beta_AA*A*(1-A) + LAMBDA_A*beta_HA*(1-A)*H + LAMBDA_A*beta_EA*(1-A)*E - mu_A*A
    dE <-  LAMBDA_E*beta_EE*E*(1-E) + LAMBDA_E*beta_HE*(1-E)*H + LAMBDA_E*beta_AE*(1-E)*A - mu_E*E
    # print(c(time,LAMBDA_H,LAMBDA_A,LAMBDA_E))
    return(  list(c(dH,dA,dE)))
  })}


AMRmodel_ENGLAND <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{ #
     
    if (time <= time1_eng)  {LAMBDA_H <- LAMBDA_H
    LAMBDA_A <- LAMBDA_H/H_A_ratio_eng } else if ((time >time1_eng) & (time < time2_eng))  {  
      LAMBDA_H <- LAMBDA_H + ((time - time1_eng)*(((LAMBDA_H*ratio_eng_2017_H) - LAMBDA_H)/(time2_eng-time1_eng)))
      LAMBDA_A <- LAMBDA_H/H_A_ratio_eng + ((time - time1_eng)*(((LAMBDA_H/H_A_ratio_eng)*ratio_eng_2017_A - LAMBDA_H/H_A_ratio_eng )/(time2_eng-time1_eng)))} else if (time>=time2_eng)   {
        LAMBDA_H <- LAMBDA_H*ratio_eng_2017_H
        LAMBDA_A <- (LAMBDA_H/H_A_ratio_eng)*ratio_eng_2017_A}
    
    dH <- gamma*LAMBDA_H*(1-H) + LAMBDA_H*beta_HH*H*(1-H) + LAMBDA_H*beta_AH*(1-H)*A + LAMBDA_H*beta_EH*(1-H)*E - mu_H*H
    dA <-  gamma*LAMBDA_A*(1-A) + LAMBDA_A*beta_AA*A*(1-A) + LAMBDA_A*beta_HA*(1-A)*H + LAMBDA_A*beta_EA*(1-A)*E - mu_A*A
    dE <-  LAMBDA_E*beta_EE*E*(1-E) + LAMBDA_E*beta_HE*(1-E)*H + LAMBDA_E*beta_AE*(1-E)*A - mu_E*E
    # print(c(time,LAMBDA_H,LAMBDA_A,LAMBDA_E))
    return(  list(c(dH,dA,dE)))
})}
