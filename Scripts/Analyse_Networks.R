# Code for Analysing Social Network


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
