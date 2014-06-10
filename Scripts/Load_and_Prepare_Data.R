# Check for whether file has already been downloaded. If it has then don't download it again


# NOTE: The code below is very temperamental: it only seems to work once per session, and does not work
# on some machines due to memory limits being reached. However I cannot improve on it for now. 


# Order of checks
# 1) Has the Robject Data been found?
# 2) Have the derived R objects Data.E and Data.V been identified?
# 3) has the CSV dataset Full_Dataset.csv been identified

########################################################################################################
########################################################################################################
############################### N E T W O R K   A N A L Y S I S ########################################
########################################################################################################
########################################################################################################
if (Network_Analysis){
    if (!file.exists("Data/RObj/Network_Data.RData")){
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
        
        print("Loading Data from CSV")
        Full_Dataset <- read.csv("Data/Raw/Full_Dataset.csv")
        
        if (Complete_Only){
            print("Removing incomplete observations")
            na_indices <- Show_NA_Row_Index(Full_Dataset)
            
            Full_Dataset <- Remove_Incomplete_Postcodes(Full_Dataset, na_indices)
            save(
                Full_Dataset,
                file="Data/RObj/Full_Dataset.RData"
                )
        }
        
    } else {
        print("Data found as R Objects. Loading workspace")
        load("Data/RObj/Full_Dataset.RData")
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
             file="Data/RObj/Network_Data.RData"
        )
    } else {
        print("All data appear to have been loaded already.")
    }    
}


############################################################################################################
############################################################################################################
################################### P A T H O S   A N A L Y S I S ##########################################
############################################################################################################
############################################################################################################
# Pathos Data from Gwilym
if (Pathos_Analysis){
    ########################################################################################################
    ####################################### P A T H O S ####################################################
    ########################################################################################################
    print("Looking for Raw(er) Pathos_Data")
    if (!file.exists("Data/RObj/Pathos_Data_Raw.RData")){
        print("Raw Pathos Data not found as R object. Searching for CSV file locally")
        if (!file.exists("Data/Raw/Pathos_Data_Raw.csv")){
            
            print("Cannot find Pathos_Data_Raw as csv. Downloading as dta")
            url <- "https://www.dropbox.com/s/8gb5vqy17pnff4u/GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta"
            dir.create("Data/Raw/", recursive=T, showWarnings=F)
            tmp <- httr::GET(
                url,
                verbose()
                )
            writeBin(
                content(tmp, "raw"),
                "Data/Raw/Pathos_Data_Raw.dta"
                )
            
            print("Downloaded Pathos_Data_Raw as dta. Loading and converting to csv")
            Pathos_Data_Raw <- foreign::read.dta(
                file="Data/Raw/Pathos_Data_Raw.dta"
                )
            write.csv(
                Pathos_Data_Raw,
                file="Data/Raw/Pathos_Data_Raw.csv"
                )
            
            print("Save Pathos_Data_Raw as RObj")
            save(
                Pathos_Data_Raw,
                file="Data/RObj/Pathos_Data_Raw.RData"
                )
            
        } else {
            print("Pathos_Raw_Data found as CSV. Loading")
            Pathos_Data_Raw <- read.csv(
                file="Data/Raw/Pathos_Data_Raw.csv"
                )
            print("Saving Pathos_Raw_Data as RObj")
            save(
                Pathos_Data_Raw,
                file="Data/RObj/Pathos_Data_Raw.RData"
                )
        }
    } else {
        print("Found Pathos_Data_Raw as RData. Loading")
        load("Data/RObj/Pathos_Data_Raw.RData")
    }
    
    ####
    if (!file.exists("Data/RObj/Pathos_Data.RData")){
        print("Pathos Data not found as R object. Searching for CSV file locally")
        
        if (!file.exists("Data/Raw/Pathos_Data.csv")){
            print("Cannot find dataset. Downloading.")            
            url <- "https://www.dropbox.com/s/cad37rmu1a3kvxa/average%20pathos_2000to2002__and__2010to2012.csv"
            dir.create("Data/Raw/", recursive=T, showWarnings=F)
            tmp <- httr::GET(
                url,
                verbose()
            )
            writeBin(
                content(tmp, "raw"),
                "Data/Raw/Pathos_Data.csv"
            )
            
            print("Reading Pathos_Data as csv, to save as RObj")
            
            Pathos_Data <- read.csv("Data/Raw/Pathos_Data.csv")
            save(
                Pathos_Data,
                file="Data/RObj/Pathos_Data.RData"
                )
        } else {
            print("Found Dataset as CSV locally, so not downloading again.")
        }
        
        
        # Read the file into an R object format
        print("Loading Data from CSV, to save as RObj")
        Pathos_Data <- read.csv("Data/Raw/Pathos_Data.csv")
        
        save(
            Pathos_Data,
            file="Data/RObj/Pathos_Data.RData"
            )
        
    } else {
        print("Data found as R Objects. Loading workspace")
        load("Data/RObj/Pathos_Data.RData")
    }    
}


# Check for existence of areal unit conversion file in correct format
##############################################################################################################
##############################################################################################################
########################  C E N S U S  -   A R E A L   U N I T   L I N K S ###################################
##############################################################################################################
##############################################################################################################
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
            print("Loading Areal_Unit_Links as sav, to write out as csv")
            
            Areal_Unit_Links <- foreign::read.spss(
                "Data/Raw/Areal_Unit_Links.sav",
                to.data.frame=T
                )
            write.csv(
                Areal_Unit_Links,
                file="Data/Raw/Areal_Unit_Links.csv"
                )
            print("Now saving Areal_Unit_Links as RObj")
            write(
                Areal_Unit_Links,
                file="Data/RObj/Areal_Unit_Links.RData"
                )
            
        } else {
            print("File found in SAV format. Converting to csv")
            Areal_Unit_Links <- foreign::read.spss(
                "Data/Raw/Areal_Unit_Links.sav",
                to.data.frame=T
            )
            write.csv(
                Areal_Unit_Links,
                file="Data/Raw/Areal_Unit_Links.csv"      
            )
            print("Now saving as RObj")
            save(
                Areal_Unit_Links,
                file="Data/RObj/Areal_Unit_Links.RData"
                )
        }
        
    } else {
        if (file.exists("Data/RObj/Areal_Unit_Links.RData")){
            print("Found Areal Unit Links as R Object. Loading now")
            load("Data/RObj/Areal_Unit_Links.RData")
        } else {
            print("Did not find Areal Unit Link file as RData. Reading from CSV")
            Areal_Unit_Links <- read.csv("Data/Raw/Areal_Unit_Links.csv")            
        }
    }    
}

if (!file.exists("Scripts/Do/Gwilyms_Pathos_File.do")){
    print("Do file not found locally. Fetching")
    url <- "https://www.dropbox.com/s/xhch0jog6pdtxg5/AQMEN_Pathos_GSPC_2jun14_v1g.do"
    dir.create("Scripts/Do/", recursive=T, showWarnings=F)
    tmp <- httr::GET(url,
                     verbose()
    )
    writeBin(
        content(tmp, "raw"),
        "Scripts/Do/Gwilyms_Pathos_File.do"
    )
    
} else {
    print("Gwilym's Do File already fetched. Doing nothing")
    
}

# Loading 2011 Census lookup
if (Aggregate_Census_OA){
    if (file.exists("Data/RObj/Census_2011_Lookup__OA_TO_HIGHER_AREAS.RData")){
        print("Found Census 2011 lookup as RObj. Loading")
        load("Data/RObj/Census_2011_Lookup__OA_TO_HIGHER_AREAS.RData")
    } else {
        print("Did not find Census 2011 Lookup as RObj. Looking for local CSV copy")
        if (file.exists("Data/Raw/Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv")){
            print("Found 2011 Census lookup table locally. Loading from CSV")
            Census_2011_Lookup__OA_TO_HIGHER_Areas <- read.csv(
                "Data/Raw/Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv"
            )    
            save(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                file="Data/RObj/Census_2011_Lookup__OA_TO_HIGHER_AREAS.RData" 
            )
            
        }   else {
            print("Did not find census 2011 Lookup as local CSV. Downloading from Dropbox")
            Census_2011_Lookup__OA_TO_HIGHER_AREAS <- repmis::source_DropboxData(
                file="Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv",
                key="95x5ozuw0c6xgxk"
            )
            write.csv(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                file="Data/Raw/Census_2011_Lookup__OA_TO_HIGHER_AREAS.csv"
            )
            save(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                file="Data/RObj/Census_2011_Lookup__OA_TO_HIGHER_AREAS.RData"
            )
        }    
    }    
}

#####################################################################################################
#####################################################################################################
############################ C E N S U S   -   V A R I A B L E S ####################################
#####################################################################################################
#####################################################################################################
if (Use_Census_Variables){
    
    #################################################################################################
    ############################## C O U N T R Y   O F   O R I G I N ################################
    #################################################################################################
    if ("Country_Of_Origin" %in% Census_Variables){
        print("Fetching 2011 Country of Origin file")
        
        ################################### 2011 ####################################################
        #############################################################################################
        if (file.exists("Data/RObj/Census_2011__KS204SC__Country_Of_Origin.RData")){
            print("2011 Country of Origin found as R Object. Loading")
            load("Data/RObj/Census_2011__KS204SC__Country_Of_Origin.RData")
            
        } else {
            print("2011 Country of Origin file not found as R Object. Looking for local CSV to load")
            if (file.exists("Data/Raw/Census_2011__KS204SC__Country_Of_Origin.csv")){
                print("2011 Country of Origin file found as csv locally. Reading")
                Census_2011__KS204SC__Country_Of_Origin <- read.csv(
                    "Data/Raw/Census_2011__KS204SC__Country_Of_Origin.csv"
                )
                
                save(
                    Census_2011__KS204SC__Country_Of_Origin,
                    file="Data/RObj/Census_2011__KS204SC__Country_Of_Origin.RData"
                )
            } else {
                print("2011 Country of Origin file not found locally. Downloading from Dropbox")
                Census_2011__KS204SC__Country_Of_Origin <- repmis::source_DropboxData(
                    file="KS204SC.csv",
                    key="kzliwt4oldfbrxl"
                )
                Census_2011__KS204SC__Country_Of_Origin <- Tidy_2011_Census_Table(Census_2011__KS204SC__Country_Of_Origin)
                write.csv(
                    Census_2011__KS204SC__Country_Of_Origin,
                    file="Data/Raw/Census_2011__KS204SC__Country_Of_Origin.csv"
                )
                save(
                    Census_2011__KS204SC__Country_Of_Origin,
                    file="Data/RObj/Census_2011__KS204SC__Country_Of_Origin.RData"
                )
            }
        }
        
        ####################################### 2001 ####################################################
        #################################################################################################
        print("Fetching 2001 Country of Origin dataset")
        
        if (file.exists("Data/RObj/Census_2001__KS005__Country_Of_Origin.RData")){
            print("2001 Country of Origin found as R Object. Loading")
            load("Data/RObj/Census_2001__KS005__Country_Of_Origin.RData")
            
        } else {
            print("2001 Country of Origin file not found as R Object. Looking for local CSV to load")
            if (file.exists("Data/Raw/Census_2001__KS005__Country_Of_Origin.csv")){
                print("2001 Country of Origin file found as csv locally. Reading")
                Census_2001__KS005__Country_Of_Origin <- read.csv(
                    "Data/Raw/Census_2001__KS005__Country_Of_Origin.csv"
                )
                
                save(
                    Census_2001__KS005__Country_Of_Origin,
                    file="Data/RObj/Census_2001__KS005__Country_Of_Origin.RData"
                )
            } else {
                print("2001 Country of Origin file not found locally. Downloading from Dropbox")
                Census_2001__KS005__Country_Of_Origin <- repmis::source_DropboxData(
                    file="Census__2001__KS005__Country_Of_Origin.csv",
                    key="erj713wnp535q20"
                )
                
                write.csv(
                    Census_2001__KS005__Country_Of_Origin,
                    file="Data/Raw/Census_2001__KS005__Country_Of_Origin.csv"
                )
                
                save(
                    Census_2001__KS005__Country_Of_Origin,
                    file="Data/RObj/Census_2001__KS005__Country_Of_Origin.RData"
                )
            }
        }
           
        
    }
    #################################################################################################
    ###################################### E T H N I C I T Y  #######################################
    #################################################################################################
    if ("Ethnicity" %in% Census_Variables){
        print("Loading Ethnicity Census Variables")

        # 2011 Table is KS201SC
        ############################### 2011 ########################################################
        #############################################################################################
        print("Loading 2011 Census Table")
        if (file.exists("Data/RObj/Census_2011__KS201SC__Ethnicity.RData")){
            print("2011 Ethnicity found as R Object. Loading")
            load("Data/RObj/Census_2011__KS201SC__Ethnicity.RData")
            
        } else {
            print("2011 file not found as R Object. Looking for local CSV to load")
            if (file.exists("Data/Raw/Census_2011__KS201SC__Ethnicity.csv")){
                print("2011  file found as csv locally. Reading")
                Census_2011__KS201SC__Ethnicity <- read.csv(
                    "Data/Raw/Census_2011__KS201SC__Ethnicity.csv"
                )
                save(
                    Census_2011__KS201SC__Ethnicity,
                    file="Data/RObj/Census_2011__KS201SC__Ethnicity.RData"
                )
            } else {
                print("2011 file not found locally. Downloading from Dropbox")
                Census_2011__KS201SC__Ethnicity <- repmis::source_DropboxData(
                    file="KS201SC.csv",
                    key="t4n19qx7kjwas9m"

                )
                Census_2011__KS201SC__Ethnicity <- Tidy_2011_Census_Table(Census_2011__KS201SC__Ethnicity)

                
                write.csv(
                    Census_2011__KS201SC__Ethnicity,
                    file="Data/Raw/Census_2011__KS201SC__Ethnicity.csv"
                )
                save(
                    Census_2011__KS201SC__Ethnicity,
                    file="Data/RObj/Census_2011__KS201SC__Ethnicity.RData"
                )
            }
        }
        
        ############################### 2001 #########################################################
        ##############################################################################################
        print("Loading 2001 Census Table")
        # 2001 Table is KS006
        if (file.exists("Data/RObj/Census_2001__KS006__Ethnicity.RData")){
            print("2001 Ethnicity found as R Object. Loading")
            load("Data/RObj/Census_2001__KS006__Ethnicity.RData")
            
        } else {
            print("2001 file not found as R Object. Looking for local CSV to load")
            if (file.exists("Data/Raw/Census_2001__KS006__Ethnicity.csv")){
                print("2001  file found as csv locally. Reading")
                Census_2001__KS006__Ethnicity <- read.csv(
                    "Data/Raw/Census_2001__KS006__Ethnicity.csv"
                )
                
                save(
                    Census_2001__KS006__Ethnicity,
                    file="Data/RObj/Census_2001__KS006__Ethnicity.RData"
                )
            } else {
                print("2001 file not found locally. Downloading from Dropbox")
                Census_2001__KS006__Ethnicity <- repmis::source_DropboxData(
                    file="Census__2001__KS006__Ethnic_Group_And_Language.csv",
                    key="kfgj930jel9qep0"
                )
                
                write.csv(
                    Census_2001__KS006__Ethnicity,
                    file="Data/Raw/Census_2001__KS006__Ethnicity.csv"
                )
                save(
                    Census_2001__KS006__Ethnicity,
                    file="Data/RObj/Census_2001__KS006__Ethnicity.RData"
                )
            }
        }
        
        
    }
    #################################################################################################
    ###################################### R E L I G I O N   ########################################
    #################################################################################################    
    if ("Religion" %in% Census_Variables){
        print("Loading Religion Census Variables")
        # 2011 Table is KS209SCb
        
        ############################### 2011 ########################################################
        #############################################################################################
        print("Loading 2011 Census table")
        if (file.exists("Data/RObj/Census_2011__KS209SCb__Religion.RData")){
            print("2011 Religion found as R Object. Loading")
            load("Data/RObj/Census_2011__KS209SCb__Religion.RData")
            
        } else {
            print("2011 file not found as R Object. Looking for local CSV to load")
            if (file.exists("Data/Raw/Census_2011__KS209SCb__Religion.csv")){
                print("2011 file found as csv locally. Reading")
                Census_2011__KS209SCb__Religion <- read.csv(
                    "Data/Raw/Census_2011__KS209SCb__Religion.csv"
                )
                
                
                save(
                    Census_2011__KS209SCb__Religion,
                    file="Data/RObj/Census_2011__KS209SCb__Religion.RData"
                )
            } else {
                print("2011 file not found locally. Downloading from Dropbox")
                Census_2011__KS209SCb__Religion <- repmis::source_DropboxData(
                    file="KS209SCb.csv",
                    key="fr03pw7x7fcawkh"
                )

                Census_2011__KS209SCb__Religion <- Tidy_2011_Census_Table(Census_2011__KS209SCb__Religion)
                
                
                write.csv(
                    Census_2011__KS209SCb__Religion,
                    file="Data/Raw/Census_2011__KS209SCb__Religion.csv"
                )
                
                save(
                    Census_2011__KS209SCb__Religion,
                    file="Data/RObj/Census_2011__KS209SCb__Religion.RData"
                )
            }
        }
        
        ############################### 2001 ########################################################
        #############################################################################################
        print("Loading 2001 census table")
        # 2001 Table is KS007
        url <- "https://www.dropbox.com/s//"
        if (file.exists("Data/RObj/Census_2001__KS007__Religion.RData")){
            print("2001 Religion found as R Object. Loading")
            load("Data/RObj/Census_2001__KS007__Religion.RData")
            
        } else {
            print("2001 file not found as R Object. Looking for local CSV to load")
            if (file.exists("Data/Raw/Census_2001__KS007__Religion.csv")){
                print("2001 file found as csv locally. Reading")
                Census_2001__KS007__Religion <- read.csv(
                    "Data/Raw/Census_2001__KS007__Religion.csv"
                )
                
                save(
                    Census_2001__KS007__Religion,
                    file="Data/RObj/Census_2001__KS007__Religion.RData"
                )
            } else {
                print("2001 file not found locally. Downloading from Dropbox")
                Census_2001__KS007__Religion <- repmis::source_DropboxData(
                    file="Census__2001__KS007__Religion.csv",
                    key="zfnhe8kwxnpupwa"
                )
                
                write.csv(
                    Census_2001__KS007__Religion,
                    file="Data/Raw/Census_2001__KS007__Religion.csv"
                )
                save(
                    Census_2001__KS007__Religion,
                    file="Data/RObj/Census_2001__KS007__Religion.RData"
                )
            }
        }
        
    }

    #############################################################################################
    ################################ C O M M O N ################################################
    #############################################################################################    
    
    print("Fetching 2001 OUtput Area Lookup")
    if (file.exists("Data/RObj/Census_2001_OA_Lookup.RData")){
        print("Found as R Object. Loading")
        load("Data/RObj/Census_2001_OA_Lookup.RData")
        
    } else {
        print("Not found as R Object. Looking for local CSV to load")
        if (file.exists("Data/Raw/Census_2001_OA_Lookup.csv")){
            print("Found as csv locally. Reading")
            Census_2001_OA_Lookup <- read.csv(
                "Data/Raw/Census_2001_OA_Lookup.csv"
            )
            
            save(
                Census_2001_OA_Lookup,
                file="Data/RObj/Census_2001_OA_Lookup.RData"
            )
        } else {
            print("Not found locally. Downloading from Dropbox")
            Census_2001_OA_Lookup <- repmis::source_DropboxData(
                file="OUTPUT_AREA_2001_LOOKUP.csv",
                key="39wszvlpxy4qvpf"
            )
            
            write.csv(
                Census_2001_OA_Lookup,
                file="Data/Raw/Census_2001_OA_Lookup.csv"
            )
            save(
                Census_2001_OA_Lookup,
                file="Data/RObj/Census_2001_OA_Lookup.RData"
            )
        }
    }
}

rm(tmp, url)

 
