source("DBDA2E-utilities.R") #"https://sites.google.com/site/doingbayesiandataanalysis/software-installation/DBDA2Eprograms.zip?attredirects=0&d=1"
source("Model_functions.R")

##*********************************##

#IBW
IBW_y_c=c(1,1,1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
IBW_y_u= c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,1,1,1,0,1,1,0,1,1,0,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,1,1,0,1,1,1,1,0,1,0,0,0,0,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,1,0,0,0,0)
#a=seq(1898,2010)
#data.frame(a,IBW_y_c,IBW_y_u)

  #Model 1
  codaSamplesIBWM1<-mcmc.list(mcmc.list(posteriotr_cer_mcmc(IBW_y_c))) 

  #Diagnostic checking
  diagMCMC( codaObject = codaSamplesIBWM1  , parName="tau" )
  diagMCMC( codaObject = codaSamplesIBWM1  , parName="theta" )
  diagMCMC( codaObject = codaSamplesIBWM1 , parName="p" )

  #Posterior Distribution
  #Start Year = 1897
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM1[,"tau"], main="" , xlim=c(42,55), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM1[,"theta"] , main="" , xlim=c(0,0.32),xlab=bquote(theta),showCurve=TRUE, HDItextPlace = 0.2)
  
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM1[,"p"] , main="" , xlim=c(0.15,0.8),xlab=bquote(p[c]),showCurve=TRUE,  HDItextPlace = 0.2)
  
  #**************************************************************************************#
  
  #Model 2
  codaSamplesIBWM2<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(IBW_y_c,IBW_y_u))) 
  
  #Diagnostic checking
  diagMCMC( codaObject=codaSamplesIBWM2  , parName="tau" )
  diagMCMC( codaObject=codaSamplesIBWM2 , parName="theta" )
  diagMCMC( codaObject=codaSamplesIBWM2 , parName="pc" )
  diagMCMC( codaObject=codaSamplesIBWM2  , parName="pui" )
  diagMCMC( codaObject=codaSamplesIBWM2  , parName="puv" )
  
  #Posterior Distribution
  #Start Year = 1897
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM2[,"tau"], main="" , xlim=c(40,60), ylim=c(0,1),xlab=bquote(tau),showCurve=FALSE, cenTend=FALSE)
  
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM2[,"theta"] , main="" , xlim=c(0,0.25),xlab=bquote(theta),showCurve=TRUE, HDItextPlace = 0.2)
  
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM2[,"pui"] , main="" , xlim=c(0,1),xlab=bquote(p[ui]),showCurve=TRUE, HDItextPlace = 0.2)
  
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM2[,"puv"] , main="" , xlim=c(0,1),xlab=bquote(p[uv]),showCurve=TRUE,  HDItextPlace = 0.2)
  
  
  openGraph(height=5,width=7)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamplesIBWM2[,"pc"] , main="" , xlim=c(0.1,0.8),xlab=bquote(p[c]),showCurve=TRUE,  HDItextPlace = 0.2)
  
##*********************************##
  
#Nukupu'u
  
Nukupu_y_c=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
Nukupu_y_u=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#a=seq(1838,2010)
#data.frame(a,Nukupu_y_c,Nukupu_y_u)

  #Model 1
  codaSamplesNukupuM1<-mcmc.list(mcmc.list(posteriotr_cer_mcmc(Nukupu_y_c))) 
  
  #Posterior Distribution
  #Start Year = 1837
  plotPost( codaSamplesNukupuM1[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
 
   #**************************************************************************************#
  
  #Model 2
  codaSamplesNukupuM2<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(Nukupu_y_c,Nukupu_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1837
  plotPost( codaSamplesNukupuM2[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)

  #Model 2(puv=pc)
  codaSamplesNukupuM3<-mcmc.list(mcmc.list(posteriotr_valid_mcmc(Nukupu_y_c,Nukupu_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1837
  plotPost( codaSamplesNukupuM3[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  
##*********************************##  
  
#Eskimo
  
Eskimo_y_c=c(1,1,1,1,1,1,0,1,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
Eskimo_y_u=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,1,0,0,0,0,0,1,0,0,1,1,1,0,0,1,0,0,0,1,0,1,0,1,1,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0)
#a=seq(1901,2010)
#data.frame(a,Eskimo_y_c,Eskimo_y_u)


  #Model 1
  codaSamplesEskimoM1<-mcmc.list(mcmc.list(posteriotr_cer_mcmc(Eskimo_y_c))) 

  #Posterior Distribution
  #Start Year = 1900
  plotPost( codaSamplesEskimoM1[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)

  #**************************************************************************************#

  #Model 2
  codaSamplesEskimoM2<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(Eskimo_y_c,Eskimo_y_u))) 

  #Posterior Distribution
  #Start Year = 1900
  plotPost( codaSamplesEskimoM2[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)

  #Model 2(puv=pc)
  codaSamplesEskimoM3<-mcmc.list(mcmc.list(posteriotr_valid_mcmc(Eskimo_y_c,Eskimo_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1900
  plotPost( codaSamplesEskimoM3[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  
   ##*********************************##
  #akialoa
  
  akialoa_y_c=c(1,rep(0,2),rep(1,3),0,1,0,0,1,1,1,rep(0,59),1,rep(0,50))
  akialoa_y_u=c(rep(0,8),rep(1,2),rep(0,38),1,rep(0,4),rep(1,1),rep(0,15),rep(1,1),rep(0,5),rep(1,1),rep(0,1),rep(1,1),rep(0,3),rep(1,1),rep(0,22),rep(1,1),rep(0,2), rep(1,1),rep(0,15))
  
  #a=seq(1888,2010,1)
  #data.frame(a, akialoa_y_c,akialoa_y_u)
  
  #Model 1
  codaSamplesakialoaM1<-mcmc.list(mcmc.list(posteriotr_cer_mcmc(akialoa_y_c))) 
  
  #Posterior Distribution
  #Start Year = 1887
  plotPost( codaSamplesakialoaM1[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  #**************************************************************************************#
  
  #Model 2
  codaSamplesakialoaM2<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(akialoa_y_c,akialoa_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1887
  plotPost( codaSamplesakialoaM2[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  
  #Model 2(puv=pc)
  codaSamplesakialoaM3<-mcmc.list(mcmc.list(posteriotr_valid_mcmc(akialoa_y_c,akialoa_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1887
  plotPost( codaSamplesakialoaM3[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
##*********************************##
  
#O`ahu 'Alauahio
  
Alauahio_y_c= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,1,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
Alauahio_y_u= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,1,0,0,1,0,0,1,1,1,1,0,1,1,0,0,1,0,1,0,0,1,0,0,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,0,0,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0)

#a=seq(1837,2010)
#data.frame(a,Alauahio_y_c,Alauahio_y_u)

  #Model 1
  codaSamplesAlauahioM1<-mcmc.list(mcmc.list(posteriotr_cer_mcmc(Alauahio_y_c))) 
  
  #Posterior Distribution
  #Start Year = 1836
  plotPost( codaSamplesAlauahioM1[,"tau"], main="" , xlim=c(60,150), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE, ROPE=c(0,174))
  
  #**************************************************************************************#
  
  #Model 2
  codaSamplesAlauahioM2<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(Alauahio_y_c,Alauahio_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1836
  plotPost( codaSamplesAlauahioM2[,"tau"], main="" , xlim=c(60,150), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE, ROPE=c(0,174))
  
  #Model 2(puv=pc)
  codaSamplesAlauahioM3<-mcmc.list(mcmc.list(posteriotr_valid_mcmc(Alauahio_y_c,Alauahio_y_u))) 
  
  #Posterior Distribution
  #Start Year = 1836
  plotPost( codaSamplesAlauahioM3[,"tau"], main="" , xlim=c(60,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  
##**********************Artificial Sighting records*****************************##
  
y_c=c(rep(1,3),rep(0,4),rep(1,3),rep(0,4),rep(1,3),rep(0,4),rep(1,3),rep(0,76))
y_u_1=c(rep(0,3),rep(1,4),rep(0,3),rep(1,4),rep(0,3),rep(1,4),rep(0,3),rep(1,6),0,rep(1,5),0,rep(1,6),0,rep(1,6),0,0,rep(1,5),0,rep(1,6),0,0,0,rep(1,2),rep(0,31))
y_u_2=c(rep(0,3),0,rep(1,2),0,rep(0,3),0,rep(1,2),0,rep(0,3),0,rep(1,2),0,rep(0,3),0,rep(1,2),rep(0,5),rep(1,2),rep(0,5),rep(1,2),rep(0,5),rep(1,2),rep(0,5),rep(1,2),rep(0,5),rep(1,2),rep(0,5),rep(1,2),rep(0,31))

  #1
  codaSamples1<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(y_c,y_u_1))) 
  plotPost( codaSamples1[,"tau"], main="" , xlim=c(0,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)

  #2
  codaSamples2<-mcmc.list(mcmc.list(posteriotr_cer_uncer_mcmc(y_c,y_u_2))) 
  plotPost( codaSamples2[,"tau"], main="" , xlim=c(0,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
  
  
 #3
  codaSamples3<-mcmc.list(mcmc.list(posteriotr_cer_mcmc(y_c))) 
  plotPost( codaSamples3[,"tau"], main="" , xlim=c(0,90), ylim=c(0,1),xlab=bquote(tau[E]),showCurve=FALSE, cenTend=FALSE)
