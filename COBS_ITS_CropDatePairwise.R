#Elizabeth Bach
#COBS ITS: Pairwise comparisons of crop*date from ADONIS
#12 Aug. 2014

#From "multivariate_tests.R"
#using adonis for the test; I pulled out non-significant interactions
#Abundance (total reads)

adonis(data.trans.rar[,-c(1:5)]~data.trans.rar$Date*data.trans.rar$Crop+data.trans.rar$SoilFrac, permutations=9999)
                                        # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# data.trans.rar$Date                      1     0.767 0.76746  2.2292 0.01970 0.0004 ***
# data.trans.rar$Crop                      2     4.442 2.22111  6.4515 0.11405 0.0001 ***
# data.trans.rar$SoilFrac                  4     1.523 0.38065  1.1057 0.03909 0.1535    
# data.trans.rar$Date:data.trans.rar$Crop  2     1.231 0.61566  1.7883 0.03161 0.0004 ***
# Residuals                               90    30.985 0.34428         0.79554           
# Total                                   99    38.949                 1.00000           
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#This compares dispersion of the groupings
mds.dist<-metaMDSdist(decostand(data.trans.rar[,-c(1:6)],"pa" ),k=6,autotransform=FALSE)
CropDate.groups<-betadisper(mds.dist, data.trans.rar$Crop:data.trans.rar$Date, type="median")
TukeyHSD(CropDate.groups)

#Running ADONIS for each pairwise comparison
#Looking at PF only,
data.PF<-droplevels(subset(data.trans.rar, Crop=="PF"))
adonis(data.PF[,-c(1:5)]~data.PF$Date+data.PF$SoilFrac, permutations=9999)
#Date significant effect, P=0.0002

#Looking at P only,
data.P<-droplevels(subset(data.trans.rar, Crop=="P"))
adonis(data.P[,-c(1:5)]~data.P$Date+data.P$SoilFrac, permutations=9999)
#Date marginally significant, P=0.06

#Looking at CC only,
data.CC<-droplevels(subset(data.trans.rar, Crop=="CC"))
adonis(data.CC[,-c(1:5)]~data.CC$Date+data.CC$SoilFrac, permutations=9999)
#Date sig. P=0.0018

#Looking at July only,
data.July<-droplevels(subset(data.trans.rar, Date=="Jul-12"))
adonis(data.July[,-c(1:5)]~data.July$Crop+data.July$SoilFrac, permutations=9999)

data.July<-droplevels(subset(data.trans.rar, Date=="Jul-12" & (Crop=="PF"|Crop=="P")))
adonis(data.July[,-c(1:5)]~data.July$Crop+data.July$SoilFrac, permutations=9999)
#P:PF diff, P=0.0002
data.July2<-droplevels(subset(data.trans.rar, Date=="Jul-12" & (Crop=="CC"|Crop=="P")))
adonis(data.July2[,-c(1:5)]~data.July2$Crop+data.July2$SoilFrac, permutations=9999)
#CC:P, different, P=0.0001
data.July3<-droplevels(subset(data.trans.rar, Date=="Jul-12" & (Crop=="CC"|Crop=="PF")))
adonis(data.July3[,-c(1:5)]~data.July3$Crop+data.July3$SoilFrac, permutations=9999)
#CC:PF, different, P=0.0001

#Looking at Oct only,
data.Oct<-droplevels(subset(data.trans.rar, Date=="Oct-12"))
adonis(data.Oct[,-c(1:5)]~data.Oct$Crop+data.Oct$SoilFrac, permutations=9999)

data.Oct<-droplevels(subset(data.trans.rar, Date=="Oct-12"& (Crop=="PF"|Crop=="P")))
adonis(data.Oct[,-c(1:5)]~data.Oct$Crop+data.Oct$SoilFrac, permutations=9999)
#PF:P, different, P=0.0001
data.Oct2<-droplevels(subset(data.trans.rar, Date=="Oct-12" & (Crop=="CC"|Crop=="P")))
adonis(data.Oct2[,-c(1:5)]~data.Oct2$Crop+data.Oct2$SoilFrac, permutations=9999)
#CC:P, different, P=0.0001
data.Oct3<-droplevels(subset(data.trans.rar, Date=="Oct-12" & (Crop=="CC"|Crop=="PF")))
adonis(data.Oct3[,-c(1:5)]~data.Oct3$Crop+data.Oct3$SoilFrac, permutations=9999)
#P:PF, different, P=0.0001