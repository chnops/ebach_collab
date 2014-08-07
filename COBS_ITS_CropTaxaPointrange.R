#Elizabeth Bach
#COBS ITS:  Aggregate Target Taxa Figure:  Crop response
#7 Aug 2014

#Use taxa.interest from COBS_ITS_TargetTaxaNMDS.R
#use all taxa from this file
head(taxa.interest)
taxa.long<-melt(taxa.interest, id=c("SampleName","Crop","SoilFrac","Date","Block"))
head(taxa.long)

#Note 0s still present here

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

taxa.Crop<-ddply(taxa.long, .(Crop, variable), summarise, .progress="text",mean=mean(value),
high95=boot.high(value), low95=boot.low(value))
names.Crop<-c("Limonomyces","Atheliales","Unknown Basidiomycota","Thanatephorus","Psathyrellaceae","Strophariaceae","Peziza","Bionectriaceae","Glomerales","Operculomyces","Limonomyces","Atheliales","Unknown Basidiomycota","Thanatephorus","Psathyrellaceae","Strophariaceae","Peziza","Bionectriaceae","Glomerales","Operculomyces","Limonomyces","Atheliales","Unknown Basidiomycota","Thanatephorus","Psathyrellaceae","Strophariaceae","Peziza","Bionectriaceae","Glomerales","Operculomyces")
taxa.Crop2<-cbind(taxa.Crop,names.Crop)
head(taxa.Crop2)

ggplot(taxa.long, aes(Crop, value))+geom_boxplot()+facet_wrap(~variable, scales="free",ncol=3)+theme_bw()

ggplot(taxa.Crop2)+geom_pointrange(aes(x=Crop,y=mean,ymin=low95,ymax=high95))+facet_wrap(~names.Crop, scales="free",ncol=5)+theme_bw()

#Looking at just the most abundante (e.g. not driven by presence <10
Crop.short<-taxa.Crop2[taxa.Crop2$names.Crop %in% c("Unknown Basidiomycota", "Strophariaceae","Psathyrellaceae","Peziza","Limonomyces"),]
head(Crop.short)

ggplot(Crop.short)+geom_pointrange(aes(x=Crop,y=mean,ymin=low95,ymax=high95))+facet_wrap(~names.Crop, scales="free",ncol=3)+theme_bw()
