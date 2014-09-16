#Elizabeth Bach
#COBS_ITS:  Diversity stats including WSprop
#4 Aug. 2014

rm(list=ls())
library(plyr)
library(vegan)

#Code originates from R. Williams (see diversity_stas.R)
#Use "ITS_WSprop.csv" for data
#? Is this data already rarified?
dataset<-read.csv(file.choose())

Richness<-fisher.alpha(dataset[,-c(1:6)],1)
head(Richness)
Shannon<-diversity(dataset[,-c(1:6)])
Evenness<-Shannon/log(Richness)
hist(Richness)
div_stats<-data.frame(dataset[,1:6],Richness,Shannon,Evenness)
head(div_stats)

#looking at data distribution
ggplot(div_stats)+geom_histogram(aes(shannons^2.7))
ggplot(div_stats)+geom_histogram(aes(richness^2.7))
ggplot(div_stats)+geom_histogram(aes(evenness^2.7))

#richness is skewed heavily to right
ggplot(div_stats)+geom_histogram(aes(richness))
#keeping log transformation helps normalize data

#Richness
summary(test<-aov(richness~Date+Crop+SoilFrac, data=div_stats))
TukeyHSD(test)
#SoilFrac is highly significant (P<0.0001), WSprop>>micro>MM, WS, LM; SM=to all fractions

#Eveness
summary(test<-aov(evenness~Date+Crop+SoilFrac, data=div_stats))
#No effects of any factor

#Shannons
summary(test<-aov(shannons~Date+Crop+SoilFrac, data=div_stats))
TukeyHSD(test)
#SoilFrac highly significant (P<0.0001), Crop on cusp (P=0.056)
#WSprop=micro=SM>MM=LM=WS;  micro>LM, but no other fraction sig.
 
#PointRange Figure
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

#Summariazing Diversity stats
head(div_stats)
Div.data<-melt(div_stats, id=c("SampleName","Date","Year","Crop","Block","SoilFrac"))
head(Div.data)

Div.sum<-ddply(Div.data, .(SoilFrac,variable), summarise,.progress="text",
mean=mean(value),
high95=boot.high(value),
low95=boot.low(value)
)
head(Div.sum)
sizes<-c(">2000",">2000",">2000","<250","<250","<250","1000-2000","1000-2000","1000-2000","250-1000","250-1000","250-1000","Whole Soil","Whole Soil","Whole Soil","WS prop","WS prop","WS prop")
Div.sum2<-data.frame(sizes, Div.sum)
head(Div.sum2)
print(levels(Div.sum2$sizes))
Div.sum2$sizes=factor(Div.sum2$sizes, levels(Div.sum2$sizes)[c(1,4,3,2,5,6)])
print(levels(Div.sum2$sizes))

ggplot(Div.sum2)+geom_pointrange(aes(x=sizes,y=mean,ymin=low95,ymax=high95), size=1)+facet_wrap(~variable, scales="free",ncol=3)+theme_bw()+
theme(aspect.ratio=1,text=element_text(face=2, size=20), panel.grid=element_blank(), panel.border=element_rect(size=3, colour="black"), legend.position="none", axis.ticks=element_line(size=2), axis.text.x=element_text(size=18, face="bold", colour="black", angle=30, vjust=0.8, hjust=0.75), strip.background=element_blank(), strip.text=element_text(size=20, face="bold"),axis.title=element_blank())

