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

# Global variables

Complete_Only <- T # Should only complete data be used? 

# Source files

source("Scripts/Functions.R")

source("Scripts/Load_and_Prepare_Data.R")





## 
##############################################################################################################
# Code using functions above as 'black boxes'

#   3) SNA of sociomatrix
#   4) Summary states of covariates 

if(!file.exists("Data/RObj/Adj_iGraph.RData")){
    print("Adjacency Graph not found. Creating from Edge Data")
    cpep.igraph <- graph.adjacency(
        Data.E,
        "undirected"
    )
    
    save(cpep.igraph, file="Data/RObj/Adj_iGraph.RData")
        
} else {
    print("Adjacency Graph file found. Loading")
    load(file="Data/RObj/Adj_iGraph.RData")
    
}

if(!file.exists("Data/RObj/Graph_Summaries.RData")){
    print("Graph summaries file not found. Creating and saving")
    #Calculate some summary graph measures: how many connected components, graph density and node closeness
    gr.no.comps<-no.clusters(cpep.igraph)
    gr.density<-graph.density(cpep.igraph)
    #node.closeness<-closeness(cpep.igraph)
    #mean(node.closeness)
    #node.betweenness<-betweenness(cpep.igraph,directed=F)
    #summary(node.betweenness)
    graph.transitivity<-transitivity(cpep.igraph,"globalundirected")
    node.centrality.sc<-centralization.betweenness(cpep.igraph, directed=F)
    #node.centrality.sc$centralization
    
    save(
        gr.no.comps, 
        gr.density,
        graph.transitivity,
        node.centrality.sc,
        
        file="Data/RObj/Graph_Summaries.RData"
        )
    
} else {
    print("Found graph summaries file so reloading")
    load("Data/RObj/Graph_Summaries.RData")
}




######################
#Homophily
#####################

if (!file.exists("Data/RObj/Homophily.RData")){
    print("Cannot find homophily calculations. Making and saving")
    Attributes.names <- colnames(Data.V)[-1]
    
    Homophily_Measures <- vector("numeric", length(Attributes.names))
    names(Homophily_Measures) <- Attributes.names
    
    for (i in Attributes.names){
        Homophily_Measures[i] <- assortativity(
            graph=cpep.igraph,
            types1=Data.V[,i],
            directed=F
        )
    }
    save(Homophily_Measures, Attributes.names, file="Data/RObj/Homophily.RData")    
    
} else {
    print("Found homophily calculations. Loading")
    load("Data/RObj/Homophily/RData")
    
}

if (!file.exists("Data/RObj/Network.RData")){
    print("Cannot find Network file. Creating and Saving")
    cpep.network <- network(
        x=Data.E,
        vertex.attr=Data.V[,-1],        
        vertex.attrnames=Attributes.names,
        
        directed=F,
        
        matrix.type="adj"
    )
    save(cpep.network, file="Data/RObj/Network.RData")
} else {
    print("Found network file. Loading")
    load("Data/RObj/Network.RData")
}




# #Create network object
# save.image("CPEPnetworkobject.RData")
# #cpep.network<-network(cpepbinary.new,vertex.attr=Ext.var.list,vertex.attrnames=Ext.var.names,directed=F,matrix.type="adjacency")
# model1<-ergm(cpep.network~edges+triangle)
# save.image("CPEP_SNA/CPEPergmRes.RData")
# 
# model2<-ergm(cpep.network~edges+triangle+absdiff("flats")+absdiff("dombed"))







