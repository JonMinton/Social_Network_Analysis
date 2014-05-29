setwd("Non-public/Housing_Project/CPEP_SNA_Results")
load("CPEPmergedwithOther.RData")
#Get rid of entries with at least 1 missing covariate
complete.dataset<-na.omit(full.datast)
#Need to extract the corresponding columns
col.select<-as.numeric(rownames(complete.dataset))+1
col.select<-c(col.select,10059:10080)
rownames(complete.dataset)<-complete.dataset[,1]
complete.dataset<-complete.dataset[,col.select]
n<-nrow(complete.dataset)
d<-ncol(complete.dataset)
#Separate into adjacency matrix and list of attributes
complete.cpep<-complete.dataset[,1:n]
cpep.attrib<-as.list(complete.dataset[,((n+1):d)])

library(statnet)
#Create network object
cpep.network<-network(complete.cpep,vertex.attr=cpep.attrib,directed=F,matrix.type="adjacency")
save.image("CPEPnetworkobject.RData")
#cpep.network<-network(cpepbinary.new,vertex.attr=Ext.var.list,vertex.attrnames=Ext.var.names,directed=F,matrix.type="adjacency")
model1<-ergm(cpep.network~edges+triangle)
save.image("CPEP_SNA/CPEPergmRes.RData")

model2<-ergm(cpep.network~edges+triangle+absdiff("flats")+absdiff("dombed"))
