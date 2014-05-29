# Check for whether file has already been downloaded. If it has then don't download it again


# NOTE: The code below is very temperamental: it only seems to work once per session, and does not work
# on some machines due to memory limits being reached. However I cannot improve on it for now. 


# Order of checks
# 1) Has the Robject Data been found?
# 2) Have the derived R objects Data.E and Data.V been identified?
# 3) has the CSV dataset Full_Dataset.csv been identified?

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
} else {
    print("Data found as R Objects. Loading workspace")
    
    load("Data/RObj/Data.RData")
    
}

print("Seeing whether data have been unpacked into Edges and Vertices")
if (!exists(Data.V) | !exists(Data.E)){
    
    if (!exists(Data.V)){
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
    if (!exists(Data.E)){
        print("Edge (Sociomatrix) data not found. Unpackaging from Data now")
        
        Data.sociomatrix <- Extract_SNA_Matrix(
            Data
        )
    }
    print("Saving unpacked data as Data.RData")
    # Saving Unpacked Data as Data.RData
    save(Data, Data.E, Data.V,
         file="Data/Robj/Data.RData"
         )
} else {
    print("All data appear to have been loaded already.")
}

