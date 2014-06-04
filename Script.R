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
require(corrgram)

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



## Correlations

tmp <- subset(
    Data_Long,
    subset=year==2001
    )
tmp2 <- tmp[,3:4]
tmp3 <- unstack(tmp2, value ~ variable)

print.xtable(    
    xtable(
        cor(tmp3),
        caption="Correlation between pathos types in 2001",
        digits=2
    ),
    type="html", 
    file="Tables/Pathos_Correlations_2001.html",
    caption.placement="top"
)

png("Figures/Pathos_Correlations_2001.png", 600, 600)
corrgram(tmp3, upper.panel=NULL, main="\nCorrelation between pathos types\n2001")
dev.off()


tmp <- subset(
    Data_Long,
    subset=year==2011
)
tmp2 <- tmp[,3:4]
tmp3 <- unstack(tmp2, value ~ variable)

print.xtable(    
    xtable(
        cor(tmp3),
        caption="Correlation between pathos types in 2011",
        digits=2
    ),
    type="html", 
    file="Tables/Pathos_Correlations_2011.html",
    caption.placement="top"
)

png("Figures/Pathos_Correlations_2011.png", 600, 600)
corrgram(tmp3, upper.panel=NULL, main="\nCorrelation between pathos types\n2011")
dev.off()


############################

tmp <- subset(
    Data_Long,
    subset=year=="dif"
)
tmp2 <- tmp[,3:4]
tmp3 <- unstack(tmp2, value ~ variable)

print.xtable(    
    xtable(
        cor(tmp3),
        caption="Correlation in difference in pathos types from 2001 to 2011",
        digits=2
    ),
    type="html", 
    file="Tables/Pathos_Correlations_diff.html",
    caption.placement="top"
)

png("Figures/Pathos_Correlations_diff.png", 600, 600)
corrgram(tmp3, upper.panel=NULL, main="\nCorrelation between pathos types\nDifference from 2001 to 2011")
dev.off()



###################### Tables ###########################################################

source("Scripts/Make_Tables.R")



###################### Figures ##########################################################

source("Scripts/Make_Figures.R")





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



###################
print.xtable(xtable(head(Data_Country_of_Origin)), file="Tables/Country_of_Origin_Format.html", type="html")
print.xtable(xtable(head(Areal_Unit_Links)), file="Tables/Areal_Unit_Link_File_Format.html", type="html")


