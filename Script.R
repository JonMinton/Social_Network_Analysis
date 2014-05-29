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


# The column numbers containing the covariates 
varcols <- 3:(dim(Data)[2] - dim(Data)[1]) + dim(Data)[1]
Data.covariates <- Extract_Covariates(
    input=Data,
    postcode.loc=2,
    cov.loc = varcols
    )
rm(varcols)


# Note: the covariates data only seems to start at row 60. A bug?

Data.sociomatrix <- Extract_SNA_Matrix(
    Data
    )



