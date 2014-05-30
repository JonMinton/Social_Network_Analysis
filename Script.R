rm(list=ls())


## PRE-REQUISITES
# Packages
require(repmis)
require(igraph)
require(plyr)
require(statnet)
require(foreign)
require(RCurl)
require(httr)
require(igraph)

# Source files

# Functions

source("Scripts/Functions.R")

source("Scripts/Load_and_Prepare_Data.R")


## 
##############################################################################################################
# Code using functions above as 'black boxes'

# To do: 
#   1) Extract sociomatrix from data [DONE]
#   2) Extract covariates only data frame [DONE]
#   3) SNA of sociomatrix
#   4) Summary states of covariates 






cpep.network<-network(complete.cpep,vertex.attr=cpep.attrib,directed=F,matrix.type="adjacency")




### Additional data promised from code:

# CPEP_SNA_Results/CPEPmergedwithOther.RData


### Additional code from Nema 2

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
# A list where all elements have the same length is a data.frame, isn't it?

#Create network object
save.image("CPEPnetworkobject.RData")
#cpep.network<-network(cpepbinary.new,vertex.attr=Ext.var.list,vertex.attrnames=Ext.var.names,directed=F,matrix.type="adjacency")
model1<-ergm(cpep.network~edges+triangle)
save.image("CPEP_SNA/CPEPergmRes.RData")

model2<-ergm(cpep.network~edges+triangle+absdiff("flats")+absdiff("dombed"))


### Additional code from Nema 3

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



cpep.igraph<-graph.adjacency(complete.cpep,"undirected")
#Calculate some summary graph measures: how many connected components, graph density and node closeness
gr.no.comps<-no.clusters(cpep.igraph)
gr.density<-graph.density(cpep.igraph)
node.closeness<-closeness(cpep.igraph)
mean(node.closeness)
node.betweenness<-betweenness(cpep.igraph,directed=F)
summary(node.betweenness)
graph.transitivity<-transitivity(cpep.igraph,"globalundirected")
graph.transitivity
node.centrality.sc<-centralization.betweenness(cpep.igraph, directed=F)
node.centrality.sc$central

######################
#Homophily
#####################
homoph.meas<-matrix(NA,1,length(cpep.attrib))
colnames(homoph.meas)<-names(cpep.attrib)
#Calculate homophily measures
for(i in 1:length(cpep.attrib))
{
    homoph.meas[1,i]<-assortativity(cpep.igraph,cpep.attrib[[i]],directed=F)
    #	print(i)
}


