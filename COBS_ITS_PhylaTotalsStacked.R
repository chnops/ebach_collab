#Elizabeth Bach
#COBS ITS Stacked Bar graphs of phyla
#July 26, 2014

library(reshape)
library(lme4)
library(lmerTest)
library(bbmle)


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

#Use data_taxa2 from "diversity_stats.R" or "COBS_ITS_TargetTaxaNMDS.R"
#Phyla-level diversity, summarizes total counts for each phylum
Phyla.data2<-ddply(data_taxa2, .(SampleName, Date, Crop, Block, SoilFrac, Phylum), summarise, .drop=FALSE, .progress="text",total=sum(value))
head(Phyla.data2)


#Mean "values"
Phyla.Crop<-ddply(Phyla.data2, .(Crop,Phylum), summarise,.progress="text",mean=mean(total),high95=boot.high(total),
low95=boot.low(total)
)
head(Phyla.Crop)

ggplot(Phyla.Crop, aes(Crop, mean, fill=Phylum))+geom_bar(stat="identity")

#Total"values"
Phyla.CropT<-ddply(Phyla.data2, .(Crop,Phylum), summarise,.progress="text",total=sum(total))
head(Phyla.CropT)
ggplot(Phyla.CropT, aes(Crop, total, fill=Phylum))+geom_bar(stat="identity")

#SoilFrac
#Total"values"
Phyla.SoilFracT<-ddply(Phyla.data2, .(SoilFrac,Phylum), summarise,.progress="text",total=sum(total))
head(Phyla.SoilFracT)
print(levels(Phyla.SoilFracT$SoilFrac))
Phyla.SoilFracT$SoilFrac=factor(Phyla.SoilFracT$SoilFrac, levels(Phyla.SoilFracT$SoilFrac)[c(2,4,3,1,5)])
print(levels(Phyla.SoilFracT$SoilFrac))

ggplot(Phyla.SoilFracT, aes(SoilFrac, total, fill=Phylum))+geom_bar(stat="identity")

#Proportional distribution of taxa
Phyla.totals<-cast(data_taxa2, SampleName + Date + Crop + Block + SoilFrac ~ Phylum, sum)
head(Phyla.totals)
Phyla.prop<-cbind(Phyla.totals[,1:5],decostand(data.frame(Phyla.totals[,-c(1:5)]),"total"))
head(Phyla.prop)
Phyla.melt<-melt(Phyla.prop, id=c("SampleName","Date","Block","Crop","SoilFrac"))
head(Phyla.melt)

#Mean normalized values (proportions)
Phyla.Crop<-ddply(Phyla.melt, .(Crop,variable), summarise,.progress="text",mean=mean(value),high95=boot.high(value),
low95=boot.low(value)
)
head(Phyla.Crop)
Phyla.Crop

ggplot(Phyla.Crop, aes(Crop, mean, fill=variable))+geom_bar(stat="identity")

Phyla.Crop.table<-data.frame(cast(Phyla.melt, variable ~ Crop, mean))
Phyla.Crop.table

#SoilFrac
Phyla.SoilFrac<-ddply(Phyla.melt, .(SoilFrac,variable), summarise,.progress="text",mean=mean(value),high95=boot.high(value),
low95=boot.low(value)
)
head(Phyla.SoilFrac)
Phyla.SoilFrac

ggplot(Phyla.SoilFrac, aes(SoilFrac, mean, fill=variable))+geom_bar(stat="identity")

Phyla.SoilFrac.table<-data.frame(cast(Phyla.melt, variable ~ SoilFrac, mean))
Phyla.SoilFrac.table
#stats don't show any SoilFrac diffs

#Run stats on normalized data
#Unknown Phyla
Phyla.null<-lmer(V1~1+(1|Block), data=Phyla.prop, REML=FALSE)
Phyla.model.full<-lmer(V1~Date*Crop*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Phyla.model.main<-lmer(V1~Date+Crop+SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Phyla.model<-lmer(V1~Date+Crop+SoilFrac+Date*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
AICtab(Phyla.null,Phyla.model.full,Phyla.model.main,Phyla.model)
anova(Phyla.null,Phyla.model.full,Phyla.model.main,Phyla.model)
#null model best fit by 4, model next best
anova(Phyla.model)
#No crop effect, Date*SoilFrac P=0.05

#Ascomycota
Asco.null<-lmer(p__Ascomycota~1+(1|Block), data=Phyla.prop, REML=FALSE)
Asco.model.full<-lmer(p__Ascomycota~Date*Crop*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Asco.model.main<-lmer(p__Ascomycota~Date+Crop+SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
AICtab(Asco.null,Asco.model.full,Asco.model.main)
anova(Asco.null,Asco.model.full,Asco.model.main)
#Main model best fit by far
anova(Asco.model.main)
#Crop highly significant, P<0.0001
difflsmeans(Asco.model.main, ddf="Satterthwaite",type=3,method.grad="simple")
#All crop different:  PF>CC>P, date also significant

#Basidiomycota
Basidio.null<-lmer(p__Basidiomycota~1+(1|Block), data=Phyla.prop, REML=FALSE)
Basidio.model.full<-lmer(p__Basidiomycota~Date*Crop*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Basidio.model.main<-lmer(p__Basidiomycota~Date+Crop+SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
AICtab(Basidio.null,Basidio.model.full,Basidio.model.main)
anova(Basidio.null,Basidio.model.full,Basidio.model.main)
#Null model and main both essentially best fit
anova(Basidio.model.main)
#Crop P=0.04, Date=0.01
difflsmeans(Basidio.model.main, ddf="Satterthwaite",type=3,method.grad="simple")
#CC>PF, P is intermediary, crop diff also

#Chytridiomycota
Chytri.null<-lmer(p__Chytridiomycota~1+(1|Block), data=Phyla.prop, REML=FALSE)
Chytri.model.full<-lmer(p__Chytridiomycota~Date*Crop*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Chytri.model.main<-lmer(p__Chytridiomycota~Date+Crop+SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
AICtab(Chytri.null,Chytri.model.full,Chytri.model.main)
anova(Chytri.null,Chytri.model.full,Chytri.model.main)
#Main model and null essentially the same
anova(Chytri.model.main)
#Crop and SoilFrac on cusp, P=0.05, P=0.07

#Glomeromycota
Glom.null<-lmer(p__Glomeromycota~1+(1|Block), data=Phyla.prop, REML=FALSE)
Glom.model.full<-lmer(p__Glomeromycota~Date*Crop*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Glom.model.main<-lmer(p__Glomeromycota~Date+Crop+SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
AICtab(Glom.null,Glom.model.full,Glom.model.main)
anova(Glom.null,Glom.model.full,Glom.model.main)
#Null model best fit by 8
anova(Glom.model.main)
#No effects

#Zygomycota
Zygo.null<-lmer(p__Zygomycota~1+(1|Block), data=Phyla.prop, REML=FALSE)
Zygo.model.full<-lmer(p__Zygomycota~Date*Crop*SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
Zygo.model.main<-lmer(p__Zygomycota~Date+Crop+SoilFrac+(1|Block), data=Phyla.prop, REML=FALSE)
AICtab(Zygo.null,Zygo.model.full,Zygo.model.main)
anova(Zygo.null,Zygo.model.full,Zygo.model.main)
#Null and main model essentially same
anova(Zygo.model.main)
#SoilFrac on cusp, P=0.07, no real differences