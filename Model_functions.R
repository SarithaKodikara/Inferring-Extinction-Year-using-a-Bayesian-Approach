#***************************************************************************#
## Model 1##
posteriotr_cer_mcmc<- function(y_c){
  require(rjags)               # Must have previously installed package rjags.
  library(coda)
  set.seed(1234)
  
  Tt=length(y_c)
  n=sum(y_c)
  t_n=0
  i=1
  while(sum(y_c[i:Tt])>0){
    t_n=i
    i=i+1                                                                                                                                                                                                                                                                                                    
  }
  
  
  lik<-c()
  
  dataList = list(    # Put the information into a lis
    t_n=t_n,
    Tt=Tt,
    n=n,
    lik=lik
    
  )
  
  # Define the model:
  modelStringm1 = paste0("
                        data {
                        C <- 1000000000 # JAGS does not warn if too small!
                        
                        ones <- 1
                        
                        
                        
                        }
                        
                        model {
                        
                        
                        for(t in 1:(t_n)){
                        lik[t]= 10^(-100)
                        }
                        
                        for(t in (t_n+1):(Tt)){
                        
                        lik[t]=p^n*((1-p))^(t-1-n)
                        }
                        
                        lik[Tt+1]= p^n*((1-p))^(Tt-n)
                        
                        x=step(Tt-tau)*(tau)+step(tau-Tt-1)*(Tt+1)
                        likelihood = lik[x]
                        
                        
                        spy <-(likelihood) /C
                        
                        ones~ dbern( spy )
                        
                        tau_0 ~ dnegbin(theta,1) 
                        tau=tau_0+1
                        theta~dunif(0,1)
                        p~dunif(0,1)
                        
                        
                        }
                        
                        ") # close quote for modelString
  
  
  
  writeLines( modelStringm1 , con="model_m1.txt" )
  
  # Run the chains:
  jagsModelm1= jags.model( file="model_m1.txt" , data=dataList, n.chains=4 , n.adapt=60000)
  update( jagsModelm1 , n.iter=60000 )
  codaSamplesm1 = coda.samples( jagsModelm1 , variable.names=c("tau","p","theta") ,n.iter=130000 , thin = 13)
  return(mcmc(codaSamplesm1))
  
}

#***************************************************************************#
## Model 2##

posteriotr_cer_uncer_mcmc<- function(y_c,y_u){
  require(rjags)               # Must have previously installed package rjags.
  library(coda)
  set.seed(1234)
  
  n_c=sum(y_c)
  n_u=sum(y_u)
  Tt=length(y_c)
  
  t_n=0
  i=1
  while(sum(y_c[i:Tt])>0){
    t_n=i
    i=i+1                                                                                                                                                                                                                                                                                                    
  }
  
  n_u_tau=c()
  
  for(i in 1:Tt){
    n_u_tau[i]=sum(y_u[1:i])
  }
  
  lik<-c()
  
  dataList = list(    # Put the information into a lis
    t_n=t_n,
    Tt=Tt,
    nc=n_c,
    n_u=n_u,
    n_u_tau=n_u_tau,
    lik=lik
    
  )
  
  
  
  # Define the model:
  modelStringm2 = paste0("
                        data {
                        C <- 1000000000 # JAGS does not warn if too small!
                        
                        ones <- 1
                        
                        
                        
                        }
                        
                        model {
                        
                        
                        for(t in 1:(t_n)){
                        lik[t]= 10^(-100)
                        }
                        
                        for(t in (t_n+1):(Tt)){
                        
                        lik[t]=pc^nc*((1-pc)*pu)^n_u_tau[t-1]* ((1-pc)*(1-pu))^(t-1-nc-n_u_tau[t-1])*
                        pui^(n_u-n_u_tau[t-1])*((1-pui))^(Tt-(t-1)-(n_u-n_u_tau[t-1]))
                        }
                        
                        lik[Tt+1]= pc^nc*((1-pc)*pu)^n_u* ((1-pc)*(1-pu))^(Tt-nc-n_u)
                        
                        x=step(Tt-tau)*(tau)+step(tau-Tt-1)*(Tt+1)
                        likelihood = lik[x]
                        
                        
                        spy <-(likelihood) /C
                        
                        ones~ dbern( spy )
                        
                        tau_0 ~ dnegbin(theta,1) 
                        tau=tau_0+1
                        theta~dunif(0,1)
                        
                        pu= puv*(1-pui)+pui*(1-puv)+puv*pui
                        pui~dunif(0,1)
                        puv~dunif(0,1)
                        pc~dunif(0,1)
                        
                        
                        }
                        
                        ") # close quote for modelString
  
  
  
  writeLines( modelStringm2 , con="model_m2.txt" )
  
  # Run the chains:
  jagsModelm2= jags.model( file="model_m2.txt" , data=dataList, n.chains=4 , n.adapt=60000)
  update( jagsModelm2 , n.iter=60000 )
  codaSamplesm2 = coda.samples( jagsModelm2 , variable.names=c("tau","pc","pui", "puv","theta") ,n.iter=130000 , thin = 13)
  return(mcmc(codaSamplesm2))
  
}

#***************************************************************************#
## Model 2 with puv=pc##

posteriotr_valid_mcmc<- function(y_c,y_u){
  require(rjags)               # Must have previously installed package rjags.
  library(coda)
  set.seed(1234)
  
  n_c=sum(y_c)
  n_u=sum(y_u)
  Tt=length(y_c)
  
  t_n=0
  i=1
  while(sum(y_c[i:Tt])>0){
    t_n=i
    i=i+1                                                                                                                                                                                                                                                                                                    
  }
  
  n_u_tau=c()
  
  for(i in 1:Tt){
    n_u_tau[i]=sum(y_u[1:i])
  }
  
  lik<-c()
  
  dataList = list(    # Put the information into a lis
    t_n=t_n,
    Tt=Tt,
    nc=n_c,
    n_u=n_u,
    n_u_tau=n_u_tau,
    lik=lik
    
  )
  
  
  
  # Define the model:
  modelStringm3 = paste0("
                        data {
                        C <- 1000000000 # JAGS does not warn if too small!
                        
                        ones <- 1
                        
                        
                        
                        }
                        
                        model {
                        
                        
                        for(t in 1:(t_n)){
                        lik[t]= 10^(-100)
                        }
                        
                        for(t in (t_n+1):(Tt)){
                        
                        lik[t]=pc^nc*((1-pc)*pu)^n_u_tau[t-1]* ((1-pc)*(1-pu))^(t-1-nc-n_u_tau[t-1])*
                        pui^(n_u-n_u_tau[t-1])*((1-pui))^(Tt-(t-1)-(n_u-n_u_tau[t-1]))
                        }
                        
                        lik[Tt+1]= pc^nc*((1-pc)*pu)^n_u* ((1-pc)*(1-pu))^(Tt-nc-n_u)
                        
                        x=step(Tt-tau)*(tau)+step(tau-Tt-1)*(Tt+1)
                        likelihood = lik[x]
                        
                        
                        spy <-(likelihood) /C
                        
                        ones~ dbern( spy )
                        
                        tau_0 ~ dnegbin(theta,1) 
                        tau=tau_0+1
                        theta~dunif(0,1)
                        
                        pu= puv*(1-pui)+pui*(1-puv)+puv*pui
                        pui~dunif(0,1)
                        puv=pc
                        pc~dunif(0,1)
                        
                        
                        }
                        
                        ") # close quote for modelString
  
  
  
  writeLines( modelStringm3 , con="model_m3.txt" )
  
  # Run the chains:
  jagsModelm3= jags.model( file="model_m3.txt" , data=dataList, n.chains=4 , n.adapt=60000)
  update( jagsModelm3 , n.iter=60000 )
  codaSamplesm3 = coda.samples( jagsModelm3 , variable.names=c("tau","pc","pui", "puv","theta") ,n.iter=130000 , thin = 13)
  return(mcmc(codaSamplesm3))
  
}
