# Code for checking if network objects exist, and if not making them


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



