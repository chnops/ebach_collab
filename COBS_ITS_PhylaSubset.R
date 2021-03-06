#Elizabeth Bach, Ryan Williams
#COBS Aggregate ITS data
#Subsetting by Phyla and looking within
#10 July 2014

library(plyr)
library(ggplot2)

#Uses "data_taxa" file generated in "diversity_stats.R" file

head(data_taxa)
data_taxa2<-data_taxa[ which(data_taxa$value>0),]
head(data_taxa2)

Zygo.data<-droplevels(subset(data_taxa, data_taxa$Phylum=="p__Zygomycota"))
str(Zygo.data)
head(Zygo.data)

Glom.data<-droplevels(subset(data_taxa, data_taxa$Phylum=="p__Glomeromycota"))
str(Glom.data)
head(Glom.data)

Asco.data<-droplevels(subset(data_taxa, data_taxa$Phylum=="p__Ascomycota"))
str(Asco.data)
head(Asco.data)

Basidio.data<-droplevels(subset(data_taxa, data_taxa$Phylum=="p__Basidiomycota"))
str(Basidio.data)
head(Basidio.data)

Chytri.data<-droplevels(subset(data_taxa, data_taxa$Phylum=="p__Chytridiomycota"))
str(Chytri.data)
head(Chytri.data)

Unk.data<-subset(data_taxa, data_taxa$Phylum=="")
head(Unk.data)
levels(Unk.data$Phylum)
str(Unk.data)

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


#Basidiomycota
#Crop
Basidio.Crop<-ddply(Basidio.data, .(Crop), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),N=length(value),SE=(sd(value)/sqrt(N-1)),
low95=boot.low(value)
)
Basidio.Crop

#order
#SoilFrac
Basidio.order<-ddply(Basidio.data, .(SoilFrac, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value), 
)
head(Basidio.order)

ggplot(Basidio.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Crop
Basidio.order<-ddply(Basidio.data, .(Crop, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Basidio.order)

ggplot(Basidio.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=Crop),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Date
#Crop
Basidio.order<-ddply(Basidio.data, .(Date, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Basidio.order)

ggplot(Basidio.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=Date),position=position_dodge(width=1))+coord_flip()+scale_y_log10()


#Family
Basidio.family<-ddply(Basidio.data, .(SoilFrac, Family), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Basidio.family)

ggplot(Basidio.family)+geom_pointrange(aes(x=Family,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Species
Basidio.species<-ddply(Basidio.data, .(SoilFrac, Species), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Basidio.species)

ggplot(Basidio.species)+geom_pointrange(aes(x=Species,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Ascomycota
#Crop
Asco.Crop<-ddply(Asco.data, .(Crop), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value),N=length(value),SE=(sd(value)/sqrt(N-1))
)
Asco.Crop

#Date
Asco.Date<-ddply(Asco.data, .(Date), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
Asco.Date

#order
Asco.order<-ddply(Asco.data, .(SoilFrac, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Asco.order)

ggplot(Asco.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Crop
Asco.order<-ddply(Asco.data, .(Crop, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Asco.order)

ggplot(Asco.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=Crop),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Date
Asco.order<-ddply(Asco.data, .(Date, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Asco.order)

ggplot(Asco.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=Date),position=position_dodge(width=1))+coord_flip()+scale_y_log10()



#Family
Asco.family<-ddply(Asco.data, .(SoilFrac, Family), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Asco.family)

ggplot(Asco.family)+geom_pointrange(aes(x=Family,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Species
Asco.species<-ddply(Asco.data, .(SoilFrac, Genus), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Asco.species)
list(unique(Asco.species$Genus))

ggplot(Asco.species)+geom_pointrange(aes(x=Species,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Glomeromycota
Glom.Crop<-ddply(Glom.data, .(Crop), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value),N=length(value),SE=(sd(value)/sqrt(N-1))
)
Glom.Crop

#Order
Glom.order<-ddply(Glom.data, .(SoilFrac, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Glom.order)

ggplot(Glom.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Glomerales
Glom.glom<-subset(Glom.data, Glom.data$Order=="o__Glomerales")
Glom.glom
Gloms.Crop<-ddply(Glom.glom, .(Crop), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value),N=length(value),SE=(sd(value)/sqrt(N-1))
)
head(Gloms.Crop)

#Family
Glom.family<-ddply(Glom.data, .(SoilFrac, Family), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Glom.family)

ggplot(Glom.family)+geom_pointrange(aes(x=Family,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Species
Glom.species<-ddply(Glom.data, .(SoilFrac, Species), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Glom.species)

ggplot(Glom.species)+geom_pointrange(aes(x=Species,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Zygomycota
Zygo.Crop<-ddply(Zygo.data, .(Crop), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value),N=length(value),SE=(sd(value)/sqrt(N-1))
)
head(Zygo.Crop)

#order
Zygo.order<-ddply(Zygo.data, .(SoilFrac, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Zygo.order)

ggplot(Zygo.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Family
Zygo.family<-ddply(Zygo.data, .(SoilFrac, Family), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Zygo.family)

ggplot(Zygo.family)+geom_pointrange(aes(x=Family,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Species
Zygo.species<-ddply(Zygo.data, .(SoilFrac, Species), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Zygo.species)

ggplot(Zygo.species)+geom_pointrange(aes(x=Species,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Chytridiomycota
Chytri.Crop<-ddply(Chytri.data, .(Crop), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value),N=length(value),SE=(sd(value)/sqrt(N-1))
)
head(Chytri.Crop)

#order
Chytri.order<-ddply(Chytri.data, .(SoilFrac, Order), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Chytri.order)

ggplot(Chytri.order)+geom_pointrange(aes(x=Order,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Family
Chytri.family<-ddply(Chytri.data, .(SoilFrac, Family), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Chytri.family)

ggplot(Chytri.family)+geom_pointrange(aes(x=Family,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()

#Species
Chytri.species<-ddply(Chytri.data, .(SoilFrac, Species), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Chytri.species)

ggplot(Chytri.species)+geom_pointrange(aes(x=Species,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()



