rm(list=ls())


## PRE-REQUISITES
# Packages
require(repmis)
require(igraph)
require(plyr)
require(statnet)
require(foreign)

# Source files

# Functions

source("Scripts/Functions.R")



# Check if data have been loaded locally, otherwise load in from Dropbox


if (file.exists("Data/Local_Data.RData")){
    print("Data already found. Will load now.")
    load("Data/Local_Data.RData")
    
    } else {
        print("Data not found locally. Fetching from dropbox")
        
        url2 <- paste0(
            "https://dl.dropboxusercontent.com/s/3tqfbty79i2hpdl/",
            "full.dataset.csv?dl=1&token_hash=AAFLlKf9IpAFOq3iVC7DQIJdeApPuTZKcG6ls5ISMCOepg&expiry=1400518143"
        )
                
        # SHA-1 is : 
        # 47e1058314f00964e5780f2aad18d5d0c3c2e49d
        
        Data <- source_data(
            url2,
            sep=","
        )
        save(Data, file="Data/Local_Data.RData")   
    }

## 
##############################################################################################################
# Code using functions above as 'black boxes'

# To do: 
#   1) Extract sociomatrix from data [DONE]
#   2) Extract covariates only data frame [DONE]
#   3) SNA of sociomatrix
#   4) Summary states of covariates 


# The column numbers containing the covariates 
varcols <- 3:(dim(Data)[2] - dim(Data)[1]) + dim(Data)[1]
Data.covariates <- Extract_Covariates(
    input=Data,
    postcode.loc=2,
    cov.loc = varcols
    )
rm(varcols)

Data.sociomatrix <- Extract_SNA_Matrix(
    Data
    )



