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

library(igraph)
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
