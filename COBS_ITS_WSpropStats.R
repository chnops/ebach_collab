#Elizabeth Bach
#COBS_ITS:  Diversity stats including WSprop
#4 Aug. 2014

library(plyr)

#Code originates from R. Williams (see diversity_stas.R)
#Use "ITS_WSprop.csv" for data
#? Is this data already rarified?
dataset<-read.csv(file.choose())

richness<-fisher.alpha(dataset[,-c(1:6)],1)
head(richness)
shannons<-diversity(dataset[,-c(1:6)])
evenness<-shannons/log(richness)
hist(richness)
div_stats<-data.frame(dataset[,1:6],richness,shannons,evenness)
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
print(levels(Div.sum$SoilFrac))
Div.sum$SoilFrac=factor(Div.sum$SoilFrac, levels(Div.sum$SoilFrac)[c(2,4,3,1,5,6)])
print(levels(Div.sum$SoilFrac))

ggplot(Div.sum)+geom_pointrange(aes(x=SoilFrac,y=mean,ymin=low95,ymax=high95))+facet_wrap(~variable, scales="free",ncol=3)+theme_bw()+theme(aspect.ratio=1)
