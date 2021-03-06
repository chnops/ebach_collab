library(lme4)
library(lmerTest)
library(bbmle)
library(reshape)

# this needs data.nosing.rar from data_wrangling.R

head(data.nosing.rar[,1:10])

#calculating richness, shannons, and evenness

richness<-fisher.alpha(data.nosing.rar[,-c(1:5)],1)
head(richness)
shannons<-diversity(data.nosing.rar[,-c(1:5)])
evenness<-shannons/log(richness)
hist(richness)
div_stats<-data.frame(data.nosing.rar[,1:5],richness,shannons,evenness)


head(div_stats)

#looking at data distribution
#ggplot(div_stats)+geom_histogram(aes(shannons^2.7))
#ggplot(div_stats)+geom_histogram(aes(richness^2.7))
#ggplot(div_stats)+geom_histogram(aes(evenness^2.7))

#richness is skewed heavily to right
ggplot(div_stats)+geom_histogram(aes(richness))
#keeping log transformation helps normalize data

#Testing main effects of date, crop, soil frac on diversity measures
summary(test<-aov(richness~Date+Crop+SoilFrac, data=div_stats))
TukeyHSD(test)
#Looking with block effect, no diff
summary(test<-aov(richness~Date+Crop+SoilFrac+(1|Block), data=div_stats))
#Looking with nested agg:

test2<-lmer(richness~Date*Crop*SoilFrac+(1|Block)+(1|Date/Crop/SoilFrac), data=div_stats)
test3<-lmer(richness~Date+Crop+SoilFrac+Date:Crop+(1|Block)+(1|Date/Crop/SoilFrac), data=div_stats)
summary(test3)


test4<-lmer(richness~Date*Crop*SoilFrac+(1|Block)+(1|Date/Crop/SoilFrac), data=div_stats)

summary(test2)
TukeyHSD(test)
#SoilFrac P=0.001, micro > LM, WS
#Looking at distribution among crop
Rich<-ddply(div_stats, .(Crop), summarise,.progress="text",
mean=mean(richness),
high95=boot.high(richness),
low95=boot.low(richness)
)
Rich
ggplot(Rich, aes(Crop, mean))+geom_pointrange(aes(ymax=high95, ymin=low95))

summary(test<-aov(evenness~Date+Crop+SoilFrac, data=div_stats))
TukeyHSD(test)
#no effects of any main factor

summary(test<-aov(shannons~Date+Crop+SoilFrac, data=div_stats))
TukeyHSD(test)
#SoilFrac P=0.02, micro>LM

?diversity

ggplot(div_stats)+geom_boxplot(aes(x=SoilFrac, y=richness))

head(data.nosing.rar[,1:10])
data_melt<-melt(data.nosing.rar, id=c("SampleName","Date","Block","Crop","SoilFrac"))

taxonomy<-read.csv(file.choose())
head(taxonomy)
head(data_melt)
data_taxa<-merge(data_melt,taxonomy,by.x="variable",by.y="X.OTU.ID")
head(data_taxa)
data_taxa2<-data_taxa[ which(data_taxa$value>0),]
head(data_taxa2)

#Calculating percent of unique OTUs micros contribute
SoilFrac.OTU<-ddply(data_taxa, .(SoilFrac~variable), summarise, .progress="text", total=sum(value))

library(compare)
micros<-subset(data_taxa, data_taxa$SoilFrac=="Micro"))
micros2<-droplevels(micros[which(micros$value>0),])
micro.OTU<-data.frame(num=1:1337,OTU=levels(micros2$variable))
str(micro.OTU)

Others<-data.frame(data_taxa[data_taxa$SoilFrac %in% c("LM","MM","SM","WS"),])
Others2<-droplevels(Others[which(Others$value>0),])
Others.OTU<-data.frame(num=1:1945, OTU=levels(Others2$variable))
str(Others.OTU)
Common.OTU<-merge(micro.OTU,Others.OTU, by="OTU")

head(Common.OTU[1:10,])
dim(Common.OTU)

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

library(plyr)
Phyla.data<-ddply(data_taxa2, .(SoilFrac, Phylum), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Phyla.data)

?geom_pointrange
ggplot(Phyla.data)+geom_pointrange(aes(x=Phylum,y=mean,ymax=high95,ymin=low95, color=SoilFrac),position=position_dodge(width=1))+coord_flip()+scale_y_log10()
stats$SoilFrac<-factor(stats$SoilFrac, levels=c("Micro","SM","MM","LM","WS"))
ggplot(stats)+geom_pointrange(aes(x=Phylum,y=mean,ymax=high95,ymin=low95,colour=SoilFrac),position=position_dodge(width=.5))+coord_flip()+scale_y_log10()

#SampleID to look for crop
Phyla.datac<-ddply(data_taxa2, .(Crop, Phylum), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Phyla.datac)

ggplot(Phyla.datac)+geom_pointrange(aes(x=Phylum,y=mean,ymax=high95,ymin=low95, color=Crop),position=position_dodge(width=1))+coord_flip()+scale_y_log10()
#Some interesting differences to pursue here

#SampleID to look for date
Phyla.datad<-ddply(data_taxa2, .(Date, Phylum), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Phyla.datad)

ggplot(Phyla.datad)+geom_pointrange(aes(x=Phylum,y=mean,ymax=high95,ymin=low95, color=Date),position=position_dodge(width=1))+coord_flip()+scale_y_log10()
#Asco and Zygo are higher in Oct, may not be significant.  All others essentiall the same.
