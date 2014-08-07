#Elizabeth Bach
#COBS ITS:  Aggregate Target Taxa Figure
#28 July 2014

#Use taxa.interest from COBS_ITS_TargetTaxaNMDS.R
#family Bionectriaceae (Ascomycota), genus Peziza (Ascomycota), family Psathyrellaceae (Basidiomycota), genus Thanatephorus (Basidiomycota)
#show significant effect of soil aggregate fraction

#Bootstrap functions from R. Williams
boot.high<-function(XX){
boot.mean<-numeric(1000)
for (i in 1:1000){
 boot.mean[i]<-mean(sample(XX,replace=T))
}
return(quantile(boot.mean,(0.975)))
}

boot.low<-function(XX){
boot.mean<-numeric(1000)
for (i in 1:1000){
 boot.mean[i]<-mean(sample(XX,replace=T))
}
return(quantile(boot.mean,(0.025)))
}

taxa.long<-melt(taxa.interest, id=c("SampleName","Crop","SoilFrac","Date","Block"))
head(taxa.long)
taxa.SoilFrac<-taxa.long[taxa.long$variable %in% c("f__Bionectriaceae","g__Peziza","f__Psathyrellaceae","g__Thanatephorus"),]
head(taxa.SoilFrac)
taxa.summary<-ddply(taxa.SoilFrac, .(SoilFrac, variable), summarise, .progress="text",mean=mean(value),
high95=boot.high(value), low95=boot.low(value))
head(taxa.summary) 
print(levels(taxa.summary$SoilFrac))

#This reorganizes the levels of SoilFrac to display micro, SM, MM, LM, WS rather than alphabetical order
taxa.summary$SoilFrac=factor(taxa.summary$SoilFrac, levels(taxa.summary$SoilFrac)[c(2,4,3,1,5)])
print(levels(taxa.summary$SoilFrac))

ggplot(taxa.summary)+geom_pointrange(aes(x=SoilFrac,y=mean,ymin=low95,ymax=high95))+facet_wrap(~variable, scales="free",ncol=2)+theme_bw()

#boxplot to look for potential outliers
taxa.SoilFrac$SoilFrac=factor(taxa.SoilFrac$SoilFrac, levels(taxa.SoilFrac$SoilFrac)[c(2,4,3,1,5)])
print(levels(taxa.SoilFrac$SoilFrac))

ggplot(taxa.SoilFrac, aes(SoilFrac, value))+geom_boxplot()+facet_wrap(~variable, scales="free",ncol=2)+theme_bw()

#notes:
#with 0s, errors spread out more, lose some signal
#Bionectriaceae micros:  only appears in 2 samples, PF41_micro=21, PF15_micro=3
#Thanatephorus SM:  only appears in PF15_SM=28, PF41_SM=66
#Psathyrellaceae WS:  appears in P46_WS=3, PF32_WS=151, PF41_WS=41
#Peziz LM:  appears in PF15_LM=27, PF32_LM=181