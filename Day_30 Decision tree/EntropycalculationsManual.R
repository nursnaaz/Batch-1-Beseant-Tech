rm(list=ls(all=TRUE))


#Entropy before split

enBefore=(-6/13*log(6/13,2))+
  (-7/13*log(7/13,2))

#After splitting with CCAVg

enCCLow<-(-6/8*log(6/8,2))+(-2/8*log(2/8,2))
enCCMed<-(-3/3*log(3/3,2))
enCCHigh<-(-2/2*log(2/2,2))

enCCTotal=8/13*enCCLow+
  3/13*enCCMed+
  2/13*enCCHigh

InfGainCC=enBefore-enCCTotal

rm(enCCHigh, enCCLow, enCCMed, enCCTotal)

#After splitting with Family

enF1<-(-2/4*log(2/4,2))+(-2/4*log(2/4,2))
enF2<-(-1/1*log(1/1,2))
enF3<-(-1/2*log(1/2,2))+(-1/2*log(1/2,2))
enF4<-(-3/6*log(3/6,2))+(-3/6*log(3/6,2))

enFTotal=4/13*enF1+
  1/13*enF2+
  2/13*enF3+
  6/13*enF4

InfGainF=enBefore-enFTotal

rm(enF1,enF2,enF3,enF4, enFTotal)

#Gain ratio
infContentID = (-1/13*log(1/13,2))*13
infContentcc = (-8/13*log(8/13,2))-
  3/13*log(3/13,2)-
  2/13*log(2/13,2)

infGainID <- enBefore

gainRatID <- infGainID/infContentID
gainRatcc <- InfGainCC/infContentcc



#Gini Index before splitting

giniBefore<- 1-(6/13)^2-(7/13)^2

#Gini based on CCavg (let us consider {low}, {medium,high})

giniCCLow=1-(6/8)^2-(2/8)^2
giniCCMedHigh=(1-(5/5)^2)

giniAfterl_mh=8/13*giniCCLow+
  5/13*giniCCMedHigh

#{medium}, {low, high}
giniCCMed=1-(3/3)^2
giniCCLowHigh=1-(6/10)^2-(4/10)^2

giniAfterm_lh=3/13*giniCCMed+10/13*giniCCLowHigh

#{high}, {low, medium}
giniCCHigh=1-(2/2)^2
giniCCLowMed=1-(6/11)^2-(5/11)^2

giniAfterh_lm=2/13*giniCCHigh+11/13*giniCCLowMed
