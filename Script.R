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

Data_Country_of_Origin.Census_2011 <- repmis::source_DropboxData(
    file="KS204SC.csv",
    key="kzliwt4oldfbrxl"
    )


Output_Area_Links_2001_2011 <- repmis::source_DropboxData(
    file= "2001_Output_Area_to_2011_Output_Area.csv",
    key="jhg2f7mirnz6a1z"
    )

Output_Area_Links_Old_New <- repmis::source_DropboxData(
    file= "2011_Output_Area_Code__Old_to_New.csv",
    key="pzjkortgo6dr07a"
    )

# From scotland.gov.uk

Another_Link <- repmis::source_data(
    url="http://www.scotland.gov.uk/Resource/Doc/933/0075365.txt"
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


# 
# ###################
# print.xtable(xtable(head(Data_Country_of_Origin)), file="Tables/Country_of_Origin_Format.html", type="html")
# print.xtable(xtable(head(Areal_Unit_Links)), file="Tables/Areal_Unit_Link_File_Format.html", type="html")
# 
# 



# I think I need to do the followingL

#1 Link Pathos_Data with Areal_Unit_Links 
# Join: intermed : Pathos_Data
# with: INTERMED : Areal_Unit_Links

Data_Linked <- merge(
    x=Data_Long,
    y=Areal_Unit_Links,
    by.x="intermed",
    by.y="INTERMED", 
    all.x=TRUE
    )

Data_Linked <- subset(
    Data_Linked,
    select=c("intermed", "year", "variable", "value", "PostcodeFull")
    )

# 2) Link Areal_Unit_Links with Output_Area_Links_2001_2011
# Join: PostcodeFull: Areal_Unit_Links
# with: MasterPostCode2001: OutputAreaLInks

Data_Linked$PostcodeFull <- trim(as.character(Data_Linked$PostcodeFull))
Output_Area_Links_2001_2011$MasterPostcode2001 <- trim(as.character(Output_Area_Links_2001_2011$MasterPostcode2001))


Data_Linked <- merge(
    x=Data_Linked, 
    y=Output_Area_Links_2001_2011,
    by.x="PostcodeFull",
    by.y="MasterPostcode2001",
    all.x=TRUE
    )


# 3) LInk OutputAreaLInks20012011 with Demo_Data
# Join OutputArea2011Code: OutputAreaLinks
# with : 2011 Output Areas : Demo Data
Data_Linked <- merge(
    x=Data_Linked,
    y=Data_Country_of_Origin.Census_2011,
    by.x="OutputArea2011Code",
    by.y="X"
    )


### Additional notes etc from Raw Data on Pathos

# Raw data on pathos:
#     
#     https://www.dropbox.com/s/8gb5vqy17pnff4u/GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta
# 
# Syntax file:
#     
#     https://www.dropbox.com/s/xhch0jog6pdtxg5/AQMEN_Pathos_GSPC_2jun14_v1g.do
# 
# 
# [] aggregate to postcode sector or postcode area for each year (or rolling averages of 3 years eg)
# [] social network with postcode sectors as nodes: link between nodes if correlation coefficient > 0.8 eg.
# [] merge with census data on ethnic change/country of origin to see if changes in pathos correlate with changes in neighbourhood mix
# [] test whether neighbourhood mix is a factor in explaining social network links (i.e. homophily in language networks)
# 
# Good luck!
#     
#     g

# 