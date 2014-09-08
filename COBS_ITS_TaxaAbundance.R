#Elizabeth Bach
#COBS ITS:  sample stats
#8 Sep 2014

library(plyr)

#Uses "data_taxa2" file generated in "diversity_stats.R" file
#data_taxa2 removes 0's, left 0's in for community analysis, but for simple math it throws everything off
head(data_taxa2)

#Looking at OTU richness and mean abundance in each sample
Sample.rich<-ddply(data_taxa2, .(SampleName), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Sample.rich

Sample.rich[order(Sample.rich$total),]

#Most rich:  CC12.Micro.October2012:  354 OTUs
#Least rich: P13.MM.July2012: 57 OTUs
#least rich samples had greatest mean abundance -> dominance signal

#Ranking most abundant species by crop
Taxa.Crop<-ddply(data_taxa2, .(Crop, Species), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Taxa.Crop
Taxa.corn<-subset(Taxa.Crop, Taxa.Crop$Crop=="CC")
Taxa.corn
Taxa.prairie<-subset(Taxa.Crop, Taxa.Crop$Crop=="P")
Taxa.prairieFert<-subset(Taxa.Crop, Taxa.Crop$Crop=="PF")

Taxa.corn[order(Taxa.corn$N),]
Taxa.corn[order(Taxa.corn$total, decreasing=TRUE),]
Taxa.prairie[order(Taxa.prairie$total, decreasing=TRUE),]
Taxa.prairieFert[order(Taxa.prairieFert$total, decreasing=TRUE),]

#Ranking most abundant Families by crop
Family.Crop<-ddply(data_taxa2, .(Crop, Family), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Family.Crop
Family.corn<-subset(Family.Crop, Family.Crop$Crop=="CC")
Family.corn
Family.prairie<-subset(Family.Crop, Family.Crop$Crop=="P")
Family.prairieFert<-subset(Family.Crop, Family.Crop$Crop=="PF")

Family.corn[order(Family.corn$total, decreasing=TRUE),]
Family.prairie[order(Family.prairie$total, decreasing=TRUE),]
Family.prairieFert[order(Family.prairieFert$total, decreasing=TRUE),]

#Ranking most abundant Phylas by crop
Phyla.Crop<-ddply(data_taxa2, .(Crop, Phylum), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Phyla.Crop
Phyla.corn<-subset(Phyla.Crop, Phyla.Crop$Crop=="CC")
Phyla.corn
Phyla.prairie<-subset(Phyla.Crop, Phyla.Crop$Crop=="P")
Phyla.prairieFert<-subset(Phyla.Crop, Phyla.Crop$Crop=="PF")

Phyla.corn[order(Phyla.corn$total, decreasing=TRUE),]
Phyla.prairie[order(Phyla.prairie$total, decreasing=TRUE),]
Phyla.prairieFert[order(Phyla.prairieFert$total, decreasing=TRUE),]

#Ranking most abundant phyla by agg
Phyla.Agg<-ddply(data_taxa2, .(SoilFrac, Phylum), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Phyla.Agg
Phyla.LM<-subset(Phyla.Agg, Phyla.Agg$SoilFrac=="LM")
Phyla.LM
Phyla.MM<-subset(Phyla.Agg, Phyla.Agg$SoilFrac=="MM")
Phyla.SM<-subset(Phyla.Agg, Phyla.Agg$SoilFrac=="SM")
Phyla.Micro<-subset(Phyla.Agg, Phyla.Agg$SoilFrac=="Micro")
Phyla.WS<-subset(Phyla.Agg, Phyla.Agg$SoilFrac=="WS")

Phyla.LM[order(Phyla.LM$total, decreasing=TRUE),]
Phyla.MM[order(Phyla.MM$total, decreasing=TRUE),]
Phyla.SM[order(Phyla.SM$total, decreasing=TRUE),]
Phyla.Micro[order(Phyla.Micro$total, decreasing=TRUE),]
Phyla.WS[order(Phyla.WS$total, decreasing=TRUE),]

#Ranking most abundant Families by agg
Family.Agg<-ddply(data_taxa2, .(SoilFrac, Family), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Family.Agg
Family.LM<-subset(Family.Agg, Family.Agg$SoilFrac=="LM")
Family.LM
Family.MM<-subset(Family.Agg, Family.Agg$SoilFrac=="MM")
Family.SM<-subset(Family.Agg, Family.Agg$SoilFrac=="SM")
Family.Micro<-subset(Family.Agg, Family.Agg$SoilFrac=="Micro")
Family.WS<-subset(Family.Agg, Family.Agg$SoilFrac=="WS")

Family.LM[order(Family.LM$total, decreasing=TRUE),]
Family.MM[order(Family.MM$total, decreasing=TRUE),]
Family.SM[order(Family.SM$total, decreasing=TRUE),]
Family.Micro[order(Family.Micro$total, decreasing=TRUE),]
Family.WS[order(Family.WS$total, decreasing=TRUE),]

#Ranking species by agg
Taxa.Agg<-ddply(data_taxa2, .(SoilFrac, Species), summarise,.progress="text",
total=sum(value),mean=mean(value),
N=length(value),SE=(sd(value)/sqrt(N-1)))

Taxa.Agg
Taxa.LM<-subset(Taxa.Agg, Taxa.Agg$SoilFrac=="LM")
Taxa.LM
Taxa.MM<-subset(Taxa.Agg, Taxa.Agg$SoilFrac=="MM")
Taxa.SM<-subset(Taxa.Agg, Taxa.Agg$SoilFrac=="SM")
Taxa.Micro<-subset(Taxa.Agg, Taxa.Agg$SoilFrac=="Micro")
Taxa.WS<-subset(Taxa.Agg, Taxa.Agg$SoilFrac=="WS")

Taxa.LM[order(Taxa.LM$total, decreasing=TRUE),]
Taxa.MM[order(Taxa.MM$total, decreasing=TRUE),]
Taxa.SM[order(Taxa.SM$total, decreasing=TRUE),]
Taxa.Micro[order(Taxa.Micro$total, decreasing=TRUE),]
Taxa.WS[order(Taxa.WS$total, decreasing=TRUE),]

