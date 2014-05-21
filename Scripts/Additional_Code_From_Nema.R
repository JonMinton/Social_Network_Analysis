# Some additional code from Nema
#   - loads data from .dta files on covariates

# Nema's code below:

# Data need to be tidied






# Data 

# Data Promised: 

# House price time series data: 
#Read in the data with house price times series values for quarterly time points from 2000 to 2007 (8 years total)
# data<-read.csv("postcode_inflation_17jan08.csv",header=T)

# setwd("/Volumes/nd29c/Non-public/Housing_Project")

#save.image("CPEP_SNA_Results/CPEPrawmatrix.RData")



#################################################################################
#Look at temp4 and temp6 for creating graph to apply to get graph and SNA
#################################################################################

#load("Non-public/Housing_Project/CPEPigraph.RData")

#########################
#Religous/Race Variables
#########################
Sikh<-read.dta("C2001Sikh_ONLY_pcodej_Glasgow.dta")
Sikh.d<-dim(Sikh)[1]
Sikh.in<-data.frame(pcode=Sikh[,3], Sikh=Sikh[,4])

Buddhist<-read.dta("C2001Buddhist_ONLY_pcodej_Glasgow.dta")
Buddhist.d<-dim(Buddhist)[1]
Buddhist.in<-data.frame(pcode=Buddhist[,3], Buddhist=Buddhist[,4 ])

CoS<-read.dta("C2001Ch_of_Scot_ONLY_pcodej_Glasgow.dta")
CoS.d<-dim(CoS)[1]
CoS.in<-data.frame(pcode=CoS[,3], CoS=CoS[,4])

Hindu<-read.dta("C2001Hindu_ONLY_pcodej_Glasgow.dta")
Hindu.d<-dim(Hindu)[1]
Hindu.in<-data.frame(pcode=Hindu[,3], Hindu=Hindu[,4])

Jewish<-read.dta("C2001Jewish_ONLY_pcodej_Glasgow.dta")
Jewish.d<-dim(Jewish)[1]
Jewish.in<-data.frame(pcode=Jewish[,3], Jewish=Jewish[,4])

Muslim<-read.dta("C2001Muslim_ONLY_pcodej_Glasgow.dta")
Muslim.d<-dim(Muslim)[1]
Muslim.in<-data.frame(pcode=Muslim[,3], Muslim=Muslim[,4])

NoRel<-read.dta("C2001NoRel_ONLY_pcodej_Glasgow.dta")
NoRel.d<-dim(NoRel)[1]
NoRel.in<-data.frame(pcode=NoRel[,3],NoRel=NoRel[,4])

OtherChr<-read.dta("C2001OtherChristn_ONLY_pcodej_Glasgow.dta")
OtherChr.d<-dim(OtherChr)[1]
OtherChr.in<-data.frame(pcode=OtherChr[,3],OtherChr=OtherChr[,4])

OthRel<-read.dta("C2001OthRel_ONLY_pcodej_Glasgow.dta")
OthRel.d<-dim(OthRel)[1]
OthRel.in<-data.frame(pcode=OthRel[,3],OthRel=OthRel[,4])

RCath<-read.dta("C2001RCatholic_ONLY_pcodej_Glasgow.dta")
RCath.d<-dim(RCath)[1]
RCath.in<-data.frame(pcode=RCath[,3],RCath=RCath[,4])

###################################
#Housing Characteristics Variables
###################################

detG<-read.dta("det_ONLY_pcodej_Glasgow.dta")
detG.d<-dim(detG)[1]
detG.in<-data.frame(pcode=detG[,3],detG=detG[,1])

flats<-read.dta("flats_ONLY_pcodej_Glasgow.dta")
flats.d<-dim(flats)[1]
flats.in<-data.frame(pcode=flats[,3],flats=flats[,1])

sdet<-read.dta("sdet_ONLY_pcodej_Glasgow.dta")
sdet.d<-dim(sdet)[1]
sdet.in<-data.frame(pcode=sdet[,3],sdet=sdet[,1])

pre1920<-read.dta("pre1920_ONLY_pcodej_Glasgow.dta")
pre1920.d<-dim(pre1920)[1]
pre1920.in<-data.frame(pcode=pre1920[,3],pre1920=pre1920[,1])

dombed<-read.dta("dombed_ONLY_pcodej_Glasgow.dta")
dombed.d<-dim(dombed)[1]
dombed.in<-data.frame(pcode=dombed[,3],dombed=dombed[,1])

size<-read.dta("size_ONLY_pcodej_Glasgow.dta")
size.d<-dim(size)[1]
size.in<-data.frame(pcode=size[,3],size=size[,1])

snsincdep<-read.dta("snsincdep_ONLY_pcodej_Glasgow.dta")
snsincdep.d<-dim(snsincdep)[1]
snsincdep.in<-data.frame(pcode=snsincdep[,3],snsincdep=snsincdep[,2])

snsJSAall<-read.dta("snsJSAall_ONLY_pcodej_Glasgow.dta")
snsJSAall.d<-dim(snsJSAall)[1]
snsJSAall.in<-data.frame(pcode=snsJSAall[,3],snsJSAall=snsJSAall[2])

snscrimerate<-read.dta("snscrimerate_ONLY_pcodej_Glasgow.dta")
snscrimerate.d<-dim(snscrimerate)[1]
snscrimerate.in<-data.frame(pcode=snscrimerate[,3],snscrimerate=snscrimerate[,2])

urbanedge<-read.dta("urbanedge_ONLY_pcodej_Glasgow.dta")
urbanedge.d<-dim(urbanedge)[1]
urbanedge.in<-data.frame(pcode=urbanedge[,3],urbanedge=urbanedge[,1])

cbd<-read.dta("cbd_ONLY_pcodej_Glasgow.dta")
cbd.d<-dim(cbd)[1]
cbd.in<-data.frame(pcode=cbd[,3],cbd=cbd[,1])

aroads<-read.dta("aroads_ONLY_pcodej_Glasgow.dta")
aroads.d<-dim(aroads)[1]
aroads.in<-data.frame(pcode=aroads[,3],aroads=aroads[,1])

railstat<-read.dta("railstat_ONLY_pcodej_Glasgow.dta")
railstat.d<-dim(railstat)[1]
railstat.in<-data.frame(pcode=railstat[,3],railstat=railstat[,1])

#save.image("CPEP_SNA/CPEPandOtherVars.RData")

#Record no. of obs. in each dataset
#data.dim<-rbind(c(nrow(cpepbinary),"cpepbinary"), c(aroads.d, "aroads"), c(Buddhist.d,"Buddhist"), c(cbd.d, "cbd"), c(CoS.d, "CoS"), c(detG.d, "detG"), c(dombed.d, "dombed"), c(flats.d, "flats"), c(Hindu.d, "Hindu"), c(Jewish.d, "Jewish"), c(Muslim.d,"Muslim"), c(NoRel.d, "NoRel"), c(OtherChr.d, "OtherChr"), c(pre1920.d, "pre1920"), c(railstat.d, "railstat"), c(RCath.d, "RCath"), c(sdet.d, "sdet"),  c(Sikh.d,"Sikh"), c(size.d, "size"), c(snscrimerate.d, "snscrimerate"), c(snsincdep.d, "snsincdep"), c(snsJSAall.d, "snsJSAall"), c(urbanedge.d, "urbanedge"))
#data.dim[order(data.dim[,1]),]

library(plyr)
cpep.temp<-data.frame(pcode=rownames(cpepbinary),cpep=cpepbinary)
dfs<-list(cpep.temp, aroads.in, Buddhist.in, cbd.in, CoS.in, detG.in, dombed.in, flats.in, Hindu.in, Jewish.in, Muslim.in, NoRel.in, OtherChr.in, pre1920.in, railstat.in, RCath.in, sdet.in, Sikh.in, size.in, snscrimerate.in, snsincdep.in, snsJSAall.in, urbanedge.in)
full.dataset<-join_all(dfs, pcode)
save.image("CPEP_SNA/CPEPmergedwithOther.RData")

#full.dataset<-merge_all, 
#Create a network attribute out of the graph and other variables


load("Non-public/Housing_Project/CPEPergmRes.RData")
#cpep.network<-network(cpepbinary.new,vertex.attr=Ext.var.list,vertex.attrnames=Ext.var.names,directed=F,matrix.type="adjacency")


#attach(Ext.var)
model1<-ergm(cpep.network~edges+triangle)


save.image("CPEP_SNA/CPEPergmRes.RData")

