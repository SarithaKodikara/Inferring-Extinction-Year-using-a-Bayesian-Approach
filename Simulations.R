source("DBDA2E-utilities.R") #"https://sites.google.com/site/doingbayesiandataanalysis/software-installation/DBDA2Eprograms.zip?attredirects=0&d=1"
source("Model_functions.R")

tau_true<-c()
tau_pos_median<-c()
tau_pos_hdiLow<-c()
tau_pos_hdiHigh<-c()
c_n=c()

for(k in 1:5){
  tau_true_= rgeom(1,0.025)
  
  while(tau_true_==0){
    tau_true_= rgeom(1,0.025)
  }
  tau_true[k]=tau_true_
  
  Tt=100
  pc_true=0.7
  puv_true=0.8
  pui_true=0.2
  
  pu_true= puv_true*(1-pui_true)+pui_true*(1-puv_true)+puv_true*pui_true
  
  y_c=c()
  y_u=c()
  
  for(i in 1:Tt){
    if(tau_true_<Tt){
      if(i<tau_true_){
        y_c[i]= rbinom(1,1,pc_true)
        y_u[i]= (1-y_c[i])*rbinom(1,1,pu_true)
      }
      if(i>=tau_true_){
        y_c[i]=0
        y_u[i]=rbinom(1,1,pui_true)
      }
      
    }
    else{
      y_c[i]= rbinom(1,1,pc_true)
      y_u[i]= (1-y_c[i])*rbinom(1,1,pu_true)
    }
  }
  
  while (y_c[Tt]==1 || sum(y_c)==0 ) {
    for(i in 1:Tt){
      if(tau_true_<Tt){
        if(i<tau_true_){
          y_c[i]= rbinom(1,1,pc_true)
          y_u[i]= (1-y_c[i])*rbinom(1,1,pu_true)
        }
        if(i>=tau_true_){
          y_c[i]=0
          y_u[i]=rbinom(1,1,pui_true)
        }
        
      }
      else{
        y_c[i]= rbinom(1,1,pc_true)
        y_u[i]= (1-y_c[i])*rbinom(1,1,pu_true)
      }
    }
  }
  
  
  cn=0
  c=1
  while(sum(y_c[c:Tt])>0){
    cn=c
    c=c+1                                                                                                                                                                                                                                                                                                    
  }
  
  
  
  p<-posteriotr_cer_uncer_mcmc(y_c,y_u)
  tau_pos_median_<-plotPost( p, main="" , xlim=c(0,100), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)[3]
  tau_pos_hdiLow_<-plotPost( p, main="" , xlim=c(0,100), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)[6]
  tau_pos_hdiHigh_<-plotPost( p, main="" , xlim=c(0,100), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)[7]
  
  tau_pos_median[k]=tau_pos_median_
  tau_pos_hdiLow[k]=tau_pos_hdiLow_
  tau_pos_hdiHigh[k]=tau_pos_hdiHigh_
  c_n[k]=cn
}

output<- data.frame(c_n,tau_true, tau_pos_median,tau_pos_hdiLow,tau_pos_hdiHigh)
write.csv(output, file="sim4.csv")