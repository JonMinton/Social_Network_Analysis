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

# Global variables

Complete_Only <- T # Should only complete data be used? 
Network_Analysis <- F
Pathos_Analysis <- T
# Source files

source("Scripts/Functions.R")

source("Scripts/Load_and_Prepare_Data.R")

if (Network_Analysis){
    source("Scripts/Create_Networks.R")
    source("Scripts/Analyse_Networks.R")
}




if (Pathos_Analysis){
    
# Do stuff here    
    
    
    
}



######################################################################################################################
######################################################################################################################

attach(Pathos_Data)


d_Type_I    <- Type_I_p_all_CY2011      -   Type_I_p_all_CY2001
d_Type_II   <- Type_II_p_all_CY2011     -   Type_II_p_all_CY2001
d_Type_III  <- Type_III_p_all_CY2011    -   Type_III_p_all_CY2001
d_Type_IV   <- Type_IV_p_all_CY2011     -   Type_IV_p_all_CY2001
d_Type_All  <- Type_All_p_all_CY2011    -   Type_All_p_all_CY2001
d_Type_Core <- Type_CORE_p_all_CY2011   -   Type_CORE_p_all_CY2001

Data_Change <- data.frame(
    intermed=intermed,
    year="dif",
    I=d_Type_I,
    II=d_Type_II,
    III=d_Type_III,
    IV=d_Type_IV,
    All=d_Type_All,
    Core=d_Type_Core
    )

Data_2001 <- data.frame(
    intermed=intermed,
    year="2001",
    I=Type_I_p_all_CY2001,
    II=Type_II_p_all_CY2001,
    III=Type_III_p_all_CY2001,
    IV=Type_IV_p_all_CY2001,
    All=Type_All_p_all_CY2001,
    Core=Type_CORE_p_all_CY2001
    )

Data_2011 <- data.frame(
    intermed=intermed,
    year="2011",
    I=Type_I_p_all_CY2011,
    II=Type_II_p_all_CY2011,
    III=Type_III_p_all_CY2011,
    IV=Type_IV_p_all_CY2011,
    All=Type_All_p_all_CY2011,
    Core=Type_CORE_p_all_CY2011
)

Data_Stacked <- rbind(
    Data_2001,
    Data_2011,
    Data_Change
    )

detach(Pathos_Data)
# Want the data in 'long' format

Data_Long <- reshape::melt(
    Data_Stacked, 
    id=c("intermed", "year"),
    measured=c(
        "I",
        "II",
        "III",
        "IV",
        "All", 
        "Core"
        )
    )

