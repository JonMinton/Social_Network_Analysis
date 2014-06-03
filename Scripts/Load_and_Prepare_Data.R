# Check for whether file has already been downloaded. If it has then don't download it again


# NOTE: The code below is very temperamental: it only seems to work once per session, and does not work
# on some machines due to memory limits being reached. However I cannot improve on it for now. 


# Order of checks
# 1) Has the Robject Data been found?
# 2) Have the derived R objects Data.E and Data.V been identified?
# 3) has the CSV dataset Full_Dataset.csv been identified?
if (Network_Analysis){
    if (!file.exists("Data/RObj/Data.RData")){
        print("Data not found as R object. Searching for CSV file locally")
        
        if (!file.exists("Data/Raw/Full_Dataset.csv")){
            url <- "https://www.dropbox.com/s/3tqfbty79i2hpdl/full.dataset.csv"
            print("Cannot find dataset. Downloading.")
            dir.create("Data/Raw/", recursive=T, showWarnings=F)
            tmp <- httr::GET(url,
                             verbose()
            )
            writeBin(
                content(tmp, "raw"),
                "Data/Raw/Full_Dataset.csv"
            )
        } else {
            print("Found Dataset as CSV locally, so not downloading again.")
        }
        
        
        # Read the file into an R object format
        print("Loading Data from CSV")
        
        Data <- read.csv("Data/Raw/Full_Dataset.csv")
        
        
        if (Complete_Only){
            browser()
            print("Removing incomplete observations")
            na_indices <- Show_NA_Row_Index(Data)
            
            Data <- Remove_Incomplete_Postcodes(Data, na_indices)
        }
        
    } else {
        print("Data found as R Objects. Loading workspace")
        
        load("Data/RObj/Data.RData")
        
    }
    
    print("Seeing whether data have been unpacked into Edges and Vertices")
    if (!exists("Data.V") | !exists("Data.E")){
        
        if (!exists("Data.V")){
            print("Vertex covariate data not found. Unpacking from Data now")
            # The column numbers containing the covariates 
            varcols <- 3:(dim(Data)[2] - dim(Data)[1]) + dim(Data)[1]
            Data.V <- Extract_Covariates(
                input=Data,
                postcode.loc=2,
                cov.loc = varcols
            )
            rm(varcols)
            
        } 
        if (!exists("Data.E")){
            print("Edge (Sociomatrix) data not found. Unpackaging from Data now")
            
            Data.E <- Extract_SNA_Matrix(
                Data
            )
        }
        print("Saving unpacked data as Data.RData")
        # Saving Unpacked Data as Data.RData
        dir.create("Data/RObj", recursive=T, showWarning=F)
        save(Data, Data.E, Data.V,
             file="Data/RObj/Data.RData"
        )
    } else {
        print("All data appear to have been loaded already.")
    }    
}

# Pathos Data from Gwilym
if (Pathos_Analysis){
    if (!file.exists("Data/RObj/Pathos_Data.RData")){
        print("Pathos Data not found as R object. Searching for CSV file locally")
        
        if (!file.exists("Data/Raw/Pathos_Data.csv")){
            url <- "https://www.dropbox.com/s/cad37rmu1a3kvxa/average%20pathos_2000to2002__and__2010to2012.csv"
            print("Cannot find dataset. Downloading.")
            dir.create("Data/Raw/", recursive=T, showWarnings=F)
            tmp <- httr::GET(url,
                             verbose()
            )
            writeBin(
                content(tmp, "raw"),
                "Data/Raw/Pathos_Data.csv"
            )
        } else {
            print("Found Dataset as CSV locally, so not downloading again.")
        }
        
        
        # Read the file into an R object format
        print("Loading Data from CSV")
        
        Pathos_Data <- read.csv("Data/Raw/Pathos_Data.csv")
        
        
    } else {
        print("Data found as R Objects. Loading workspace")
        
        load("Data/RObj/Pathos_Data.RData")
        
    }    
}


# Check for existence of areal unit conversion file in correct format

if (Areal_Unit_Conversion){
    if (!file.exists("Data/Raw/Areal_Unit_Links.csv")){
        # Does the file exist as a csv file?
        print("Areal Unit conversion file not found in csv. Looking for it in sav format")
        if (!file.exists("Data/Raw/Areal_Unit_Links.sav")){
            print("Areal unit file not found in sav format. Downloading it from dropbox")
            # Does the file exist as a sav file?
            # Download it
            url <- "https://www.dropbox.com/s/373otkkuo1fdpht/latestpcinfowithlinkpc.sav"
            dir.create("Data/Raw/", recursive=T, showWarnings=F)
            tmp <- httr::GET(url,
                             verbose()
            )
            writeBin(
                content(tmp, "raw"),
                "Data/Raw/Areal_Unit_Links.sav"
            )
            
        } else {
            print("File found in SAV format. Converting to csv")
            Dta <- foreign::read.spss(
                "Data/Raw/Areal_Unit_Links.sav",
                to.data.frame=T
            )
            write.csv(
                Dta,
                file="Data/Raw/Areal_Unit_Links.csv"      
            )
        }
        
    } else {
        print("File found in csv format. Loading as R object")
        Areal_Unit_Links <- read.csv("Data/Raw/Areal_Unit_Links.csv")
    }    
}


