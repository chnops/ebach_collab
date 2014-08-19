library(vegan)
blocks<-c(rep("A",6),rep("B",9),rep("C",9),rep("D",7),rep("A",9),rep("B",6),rep("C",8),rep("D",10),rep("A",9),rep("B",9),rep("C",9),rep("D",9))
as.matrix(blocks)

div_stats<-cbind(data.nosing.rar[,1:5], blocks,specnumber(data.nosing.rar[,-c(1:5)]))
div_stats

names(div_stats)[7]<-"richness"

library(lme4)

test<-lmer(richness~Date+Crop+SoilFrac+(1|blocks)+(1|Date/Crop/SoilFrac),data=div_stats)
anova(test)
library(lmerTest)
difflsmeans(test)