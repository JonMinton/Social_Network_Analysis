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
require(reshape)
require(ggplot2)
require(foreign)
require(xtable)

# Global variables

Complete_Only <- T # Should only complete data be used? 
Network_Analysis <- F
Pathos_Analysis <- T
Areal_Unit_Conversion <- T

# Source files

source("Scripts/Functions.R")

source("Scripts/Load_and_Prepare_Data.R")

if (Network_Analysis){
    source("Scripts/Create_Networks.R")
    source("Scripts/Analyse_Networks.R")
}




if (Pathos_Analysis){
    
# Do stuff here    
    source("Scripts/Make_Long_Format_Pathos_Data.R")
    
    
}






###################### Tables ###########################################################

tmp <- xtable(ddply(Data_Long, year~variable, summarise, mean=mean(value), sd=sd(value)))
print.xtable(tmp, file="Tables/Pathos_Summary.html", type="html")

###################### Figures ##########################################################

# Visualisation

png("Figures/Density_Lattice_Full.png", width=1200, height=800)
d <- ggplot(
    Data_Long,
    aes(
        x=value
        )
    )
d2 <- d + geom_density(fill="black", colour="black") + facet_grid(year ~variable)
d3 <- d2 + xlim(-0.1, 0.1) + ylim(0, 50)
print(d3)
dev.off()




types <- c(
    "I", 
    "II", 
    "III", 
    "IV",
    "All",
    "Core"
    )


Data_Long.nomiss <- Data_Long[NULL,]
# What if 0s are treated as missing

intermeds <- unique(Data_Long$intermed)

for (i in types){
    
    tmp.df <- subset(Data_Long, year==2001 & variable==i)
    tmp1 <- as.character(tmp.df$intermed[which(tmp.df$value==0)])
    tmp.df <- subset(Data_Long, year==2011 & variable==i)
    tmp2 <- as.character(tmp.df$intermed[which(tmp.df$value==0)])
    tmp3 <- union(tmp1, tmp2)
    
    tmp4 <- setdiff(intermeds, tmp3)
    tmp.df <- subset(Data_Long, variable==i)
    
    tmp.df <- tmp.df[tmp.df$intermed %in% tmp4,]
    Data_Long.nomiss <- rbind(df.nomiss, tmp.df)
    
}


png("Figures/Density_Lattice_No_Missings.png", width=1200, height=800)
d <- ggplot(
    Data_Long.nomiss,
    aes(
        x=value
    )
)
d2 <- d + geom_density(fill="black", colour="black") + facet_grid(year ~variable)
d3 <- d2 + xlim(-0.1, 0.1) + ylim(0, 50)
print(d3)
dev.off()






#####################################################################################################
############# Merging by areal unit characteristics #################################################
#####################################################################################################



#http://www.isdscotland.org/Products-and-Services/GPD-Support/Geography/Postcode-Reference-File/_docs/latestpcinfowithlinkpc.sav?1




# Correlates of intermediate geographies


# Possible things to explore:

# 1) Age distribution and logos/pathos
# 2) percent pensioners
# 3) percent children

# Income and poverty

# KS204SC - Immigrant UK
#url <- "https://www.dropbox.com/s/kzliwt4oldfbrxl/KS204SC.csv"

Data_Country_of_Origin <- source_DropboxData(
    file="KS204SC.csv",
    key="kzliwt4oldfbrxl"
    )


Df.tmp <- Areal_Unit_Links[,c("Datazone", "INTERMED")]

Data_Country_of_Origin__merged <- merge(
    x=Data_Long,
    y=Df.tmp,
    by.x="intermed",    
    by.y="INTERMED",
    all.x=TRUE
    )


Data_Country_of_Origin__merged <- merge(
    x=Data_Country_of_Origin__merged,
    y=Data_Country_of_Origin,
    by.x="intermed",
    by.y="X",
    all.x=TRUE
    )



