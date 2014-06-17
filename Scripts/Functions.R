#Extract CPEP values 
Make_CPEP <- function(
    input, 
    diagval = 1 # Should be 1 when output then passed to Make_CPEPGraph, 0 otherwise
){
    
    N <- dim(input)[1]
    output <- matrix(NA, N, N)
    
    for (i in 1:(N-1)){
        for (j in (i + 1):N){
            
            output[i,j] <- lm(
                input[,(i+1)] ~ input[,(j+1)]
            )$coef[2]

output[j,i] <- lm(
    input[,(j+1)] ~ input[,(i+1)]
)$coef[2]
        }
    }
diag(output) <- diagval
return(output)
}


Make_CPEP_Graph <- function(
    input
){
    #Want to create a graph object representing substitutability so need symmetry
    #Closer to one the coefficient is, the more substitutable the postcode dwellings
    #Given regression coefficients are not exactly reciprocals of one another (due to random error) 
    #so if the coefficient is more than one, the reciprocal is taken 
    #and the substitutability value is taken to be the maximum of one coefficient and the reciprocal of the other coefficient
    #Note those few values with coefficients less than zero were rounded to zero
    N <- dim(input)[1]
    output <- matrix(NA, N, N)
    
    for (i in 1:(N-1)){
        for (j in (i+1):N){
            tmp1 <- max(0, input[i,j])
            tmp2 <- max(0, input[j,i])
            if (tmp1 > 1 ) tmp1 <- 1/tmp1
            if (tmp2 > 1 ) tmp2 <- 1/tmp2
            output[i,j] <- output[j,i] <- max(tmp1, tmp2)            
        }
    }
    
    return(output)
}


Show_NA_Row_Index <- function(Df){
    # Thanks to :
    # http://faculty.nps.edu/sebuttre/home/R/missings.html
    
    output <- unique(
        unlist(
            lapply(
                Df,
                function(x) which(is.na(x))
                )
            )
        )
    return(output)
    
}

# Remove postcodes associated with rows where the attribute details are missing

Remove_Incomplete_Postcodes <- function(
    input,
    incompletes,
    pcode_row.loc=2
    ){
    output <- input[
        setdiff(1:dim(input)[1], incompletes),
        setdiff(1:dim(input)[2], pcode_row.loc + incompletes)
        ]
    return(output)
}

Extract_SNA_Matrix <- function(
    input, 
    pcode_row.loc=2
){
    pcode_row <- as.character(input[,pcode_row.loc])
    pcode_col <- colnames(Data)[-c(1:pcode_row.loc)]
    pcode_col <- strsplit(pcode_col, "[.]")
    pcode_col <- sapply(
        pcode_col, 
        function(x) {
            if (length(x)==3){
                paste(x[2], x[3], sep=" ")
            } else {""}
        }
    )
    pcode_col <- pcode_col[pcode_col!=""]
    
    if (!all(pcode_row==pcode_col)) stop("Postcodes on rows and columns are different.\nRecheck data")
    
    sna_cols.loc <- (pcode_row.loc+1):(pcode_row.loc + length(pcode_row))
    output <- as.matrix(input[,sna_cols.loc])
    rownames(output) <- colnames(output) <- pcode_row
    output <- as.sociomatrix(output)
    return(output)
}


# There are covariate details for each postcode on the last few columns of the dataset data.
# This function will produce another dataframe just containing this information

Extract_Covariates <- function(
    input,
    postcode.loc=2,
    cov.loc 
){
    postcodes <- input[,postcode.loc]
    covariates <- input[,cov.loc]
    output <- data.frame(postcode=postcodes, covariates)
    return(output)
}

# Given the cpep matrix, this function will convert values into a binary indicator
Make_CPEP_Binary <- function(
    input,
    cutoff=0.9
){
    diag(input) <- 0 # If the diagonals were not 0, they will be now
    N.row <- dim(input)[1]
    N.col <- dim(input)[2]
    
    output <- matrix(0, N.row, N.col)
    
    binary_locator <- which(
        input >= cutoff,
        arr.ind=T
    )
    
    N <- nrow(binary_locator)
    
    for (i in 1:N){
        output[
            binary_locator[i,1], 
            binary_locator[i,2]
            ] <- 1 
    }
    return(output)   
}



# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

Tidy_2011_Census_Table <- function(input){
    x1 <- input[,1]
    x2 <- input[,-1]
    
    
    x2 <- apply(
        x2,
        2,
        function(x) {
            out <- x
            out <- as.character(out)
            out <- sub(",", "", out)
            out <- sub("-", "0", out)
            out <- as.numeric(out)
            return(out)
        }
    )
    output <- data.frame(x1, x2)
    output <- output[-1,]
    
    return(output)
}


#########################################################################################################
################## FUNCTION FOR MANAGING LONG LINKS BETWEEN TABLES ######################################
#########################################################################################################

Long_Merge <- function(
    Origin,
    Links,    
    Target,
    all_opt=T

    ){
    # Origin is a list containing:
    #   1) DF
    #   2) Link.out
    
    # Links is a list of length according to number of intermediate links, each element containing:
    # 1 ) Link.in
    # 2 ) Link.out
    
    # Target is a list containing:
    #   1) DF
    #   2) Link.in
    
    Output <- Origin[["DF"]]
    
    Output <- merge(
        x= Output,
        y=Links[[1]][["DF"]],
        
        by.x=Origin[["Link.out"]],
        by.y=Links[[1]][["Link.in"]],
        all=all_opt

        )
    
    N.links <- length(Links)
    
    if (N.links > 1){
        for (i in 2:N.links){
            Output <- merge(
                x=Output,
                y=Links[[i]][["DF"]],
                by.x=Links[[i-1]][["Link.out"]],
                by.y=Links[[i]][["Link.in"]],
                all=all_opt

            )
            if (dim(Output)[1]==0) stop("Link Broken")
        }        
    }
    
    
    Output <- merge(
        x=Output,
        y=Target[["DF"]],
        
        by.x=Links[[N.links]][["Link.out"]],
        by.y=Target[["Link.in"]]
        )
    
    return(Output)
}




Make_Tidy_Census <- function(){
    area_links <- Census_2011_Lookup__OA_TO_HIGHER_AREAS
    
    area_links <- subset(
        area_links,
        select=c(
            "OutputArea2011Code",
            "IntermediateZone2001Code",
            "OutputArea2001Code"
        )
    )
    
    c2011_ethnicity <- Census_2011__KS201SC__Ethnicity
    
    m1 <- merge(
        x=c2011_ethnicity,
        y=area_links,
        by.x="x1",
        by.y="OutputArea2011Code",
        all.x=T,
        all.y=F
    )
    
    m2 <- gdata::remove.vars(
        m1,
        names=c(
            "OutputArea2001Code",
            "x1"
        )
    )
    
    
    # melt it 
    
    m3 <- reshape2::melt(
        m2,
        id.var="IntermediateZone2001Code"
    )
    
    m4 <- cast(
        m3,
        IntermediateZone2001Code ~ variable,
        sum
    )
    
    m5 <- cbind(m4, year=2011, demog="ethnicity")
    m6 <- melt(
        m5, 
        id.var=c(
            "IntermediateZone2001Code",
            "year",
            "demog"
        )
    )
    
    c2011_ethnicity_long <- m6
    
    rm(list=ls(pattern="^m[[:digit:]]{1}"))
    
    
    ############
    c2011_religion <- Census_2011__KS209SCb__Religion
    
    
    
    m1 <- merge(
        x=c2011_religion,
        y=area_links,
        by.x="x1",
        by.y="OutputArea2011Code",
        all.x=T,
        all.y=F
    )
    
    m2 <- gdata::remove.vars(
        m1,
        names=c(
            "OutputArea2001Code",
            "x1"
        )
    )
    
    
    # melt it 
    
    m3 <- reshape2::melt(
        m2,
        id.var="IntermediateZone2001Code"
    )
    
    m4 <- cast(
        m3,
        IntermediateZone2001Code ~ variable,
        sum
    )
    
    m5 <- cbind(m4, year=2011, demog="religion")
    m6 <- melt(
        m5, 
        id.var=c(
            "IntermediateZone2001Code",
            "year",
            "demog"
        )
    )
    
    c2011_religion_long <- m6
    c2011_long <- rbind(
        c2011_ethnicity_long,
        c2011_religion_long
    )
    rm(list=ls(pattern="^m[[:digit:]]{1}"))
    ########
    # Now for country of origin
    
    c2011_coo <- Census_2011__KS204SC__Country_Of_Origin
    # Now to do the same for country of origin and religion
    
    
    
    
    m1 <- merge(
        x=c2011_coo,
        y=area_links,
        by.x="x1",
        by.y="OutputArea2011Code",
        all.x=T,
        all.y=F
    )
    
    m2 <- gdata::remove.vars(
        m1,
        names=c(
            "OutputArea2001Code",
            "x1"
        )
    )
    
    
    # melt it 
    
    m3 <- reshape2::melt(
        m2,
        id.var="IntermediateZone2001Code"
    )
    
    m4 <- cast(
        m3,
        IntermediateZone2001Code ~ variable,
        sum
    )
    
    m5 <- cbind(m4, year=2011, demog="coo")
    m6 <- melt(
        m5, 
        id.var=c(
            "IntermediateZone2001Code",
            "year",
            "demog"
        )
    )
    
    c2011_coo_long <- m6
    
    
    c2011_long <- rbind(
        c2011_long,
        c2011_coo_long
    )
    
    
    
    rm(list=ls(pattern="^m[[:digit:]]{1}"))
    
    
    
    # Now to do the same but with 2001 data
    # (An extra link needed)
    
    #
    area_links <- Census_2011_Lookup__OA_TO_HIGHER_AREAS
    
    area_links <- subset(
        area_links,
        select=c(
            "OutputArea2011Code",
            "IntermediateZone2001Code",
            "OutputArea2001Code"
        )
    )
    
    areal_links <- merge(
        area_links,
        Census_2001_OA_Lookup,
        by="OutputArea2001Code",
        all.x=T,
        all.y=F
    )
    
    
    
    c2001_ethnicity <- Census_2001__KS006__Ethnicity
    
    
    
    m1 <- merge(
        x=c2001_ethnicity,
        y=areal_links,
        by.x="Zone.Code",
        by.y="NRSoldOutputArea2001Code",
        all.x=T,
        all.y=F
    )
    has.intermed <- which(!is.na(m1$IntermediateZone2001Code))
    m2 <- m1[has.intermed,]
    
    
    m3 <- gdata::remove.vars(
        m2,
        names=c(
            "OutputArea2001Code",
            "OutputArea2011Code",
            "Zone.Code"
        )
    )
    
    # melt it 
    
    m4 <- reshape2::melt(
        m3,
        id.var="IntermediateZone2001Code"
    )
    
    m5 <- cast(
        m4,
        IntermediateZone2001Code ~ variable,
        sum
    )
    
    m6 <- cbind(m5, year=2001, demog="ethnicity")
    m7 <- melt(
        m6, 
        id.var=c(
            "IntermediateZone2001Code",
            "year",
            "demog"
        )
    )
    
    c2001_ethnicity_long <- m7
    
    rm(list=ls(pattern="^m[[:digit:]]{1}"))
    
    ######
    
    
    # Now religion:
    
    
    c2001_religion<- Census_2001__KS007__Religion
    
    
    
    m1 <- merge(
        x=c2001_religion,
        y=areal_links,
        by.x="Zone.Code",
        by.y="NRSoldOutputArea2001Code",
        all.x=T,
        all.y=F
    )
    
    has.intermed <- which(!is.na(m1$IntermediateZone2001Code))
    m2 <- m1[has.intermed,]
    
    
    m3 <- gdata::remove.vars(
        m2,
        names=c(
            "OutputArea2001Code",
            "OutputArea2011Code",
            "Zone.Code"
        )
    )
    
    # melt it 
    
    m4 <- reshape2::melt(
        m3,
        id.var="IntermediateZone2001Code"
    )
    
    m5 <- cast(
        m4,
        IntermediateZone2001Code ~ variable,
        sum
    )
    
    m6 <- cbind(m5, year=2001, demog="religion")
    
    m7 <- melt(
        m6, 
        id.var=c(
            "IntermediateZone2001Code",
            "year",
            "demog"
        )
    )
    
    c2001_religion_long <- m7
    
    rm(list=ls(pattern="^m[[:digit:]]{1}"))
    
    c2001_long <- rbind(
        c2001_religion_long,
        c2001_ethnicity_long
    )
    
    ## now country of origin:
    
    # Now religion:
    
    
    c2001_coo<- Census_2001__KS005__Country_Of_Origin
    
    
    
    m1 <- merge(
        x=c2001_coo,
        y=areal_links,
        by.x="Zone.Code",
        by.y="NRSoldOutputArea2001Code",
        all.x=T,
        all.y=F
    )
    
    has.intermed <- which(!is.na(m1$IntermediateZone2001Code))
    m2 <- m1[has.intermed,]
    
    
    m3 <- gdata::remove.vars(
        m2,
        names=c(
            "OutputArea2001Code",
            "OutputArea2011Code",
            "Zone.Code"
        )
    )
    
    # melt it 
    
    m4 <- reshape2::melt(
        m3,
        id.var="IntermediateZone2001Code"
    )
    
    m5 <- cast(
        m4,
        IntermediateZone2001Code ~ variable,
        sum
    )
    
    m6 <- cbind(m5, year=2001, demog="coo")
    
    m7 <- melt(
        m6, 
        id.var=c(
            "IntermediateZone2001Code",
            "year",
            "demog"
        )
    )
    
    c2001_coo_long <- m7
    
    rm(list=ls(pattern="^m[[:digit:]]{1}"))
    
    c2001_long <- rbind(
        c2001_long,
        c2001_coo_long
    )
    
    c_long <- rbind(
        c2001_long,
        c2011_long
    )
    
    c_long <- gdata::rename.vars(
        c_long,
        from="IntermediateZone2001Code",
        to="intermed"
    )
    
    # c_long is the data we want, but the variable names need to be tidied
    
    # TO DO: TIDY VARIABLE NAMES
    # c_long variable names need harmonising using recode function
    
    c_long2 <- c_long
    
    f1  <- function(x){
        out <- table(x$variable)
        out <- out[out > 0]
        return(out)
    }
    
    dlply(c_long, .(demog, year), f1)
    
    
    # religion first
    c_long2$variable <- car::recode(
        c_long2$variable,
        recodes="
        c('All_People', 'All.people') = 'total';
        c('Church_Of_Scotland', 'Church.of.Scotland') = 'christian.cos';
        c('Roman_Catholic', 'Roman.Catholic') = 'christian.catholic';
        c('Other_Christian', 'Other.Christian') = 'christian.other';
        'Buddhist' = 'buddhist';
        'Hindu' = 'hindu';
        'Muslim' = 'muslim';
        'Sikh' = 'sikh';
        'Jewish' = 'jewish';
        c('Another_Religion', 'Other.Religion') = 'other_religion';
        c('None', 'No.religion') = 'no_religion';
        c('Not_Announced', 'Religion.not.stated') = 'unknown_religion'
        "
    )
    
    # ethnicity now
    c_long2$variable <- car::recode(
        c_long2$variable,
        recodes="
        c('All_People', 'All.people') = 'total';
        c('White_Scottish', 'White..Scottish') = 'white.scottish';
        c('Other_White_British', 'White..Other.British') = 'white.british_other';
        c('White_Irish', 'White..Irish') = 'white.irish';
        c('Other_White', 'White..Other.White') = 'white.other';
        c('Any_Mixed_Background', 'Mixed.or.multiple.ethnic.groups') = 'any_mixed';
        c('Indian', 'Asian..Asian.Scottish.or.Asian.British..Indian..Indian.Scottish.or.Indian.British') = 'indian';
        c('Pakistani', 'Asian..Asian.Scottish.or.Asian.British..Pakistani..Pakistani.Scottish.or.Pakistani.British') = 'pakistani';
        c('Bangladeshi', 'Asian..Asian.Scottish.or.Asian.British..Bangladeshi..Bangladeshi.Scottish.or.Bangladeshi.British') = 'bangladeshi';
        c('Other_South_Asian', 'Asian..Asian.Scottish.or.Asian.British..Other.Asian') = 'other_south_asian';
        c('Chinese', 'Asian..Asian.Scottish.or.Asian.British..Chinese..Chinese.Scottish.or.Chinese.British') = 'chinese';
        c(
        'African',
        'Black_Scottish_Or_Other_Black',
        'Caribbean',
        'African..African..African.Scottish.or.African.British',
        'African..Other.African',
        'Caribbean.or.Black',
        'Caribbean.or.Black..Caribbean..Caribbean.Scottish.or.Caribbean.British',
        'Caribbean.or.Black..Black..Black.Scottish.or.Black.British',
        'Caribbean.or.Black..Other.Caribbean.or.Black'
        ) = 'black_afrocaribbean';
        c('Other_Ethnic_Group', 'Other.ethnic.groups') = 'other_ethnic_group'
        "
    )
    
    # problem groups include:
    rows.to.drop <- c_long2$variable %in% c(
        "Gaelic_Speaker_And_Born_In_Scotland",
        "Gaelic_Speaker_And_Not_Born_In_Scotland",
        "Asian..Asian.Scottish.or.Asian.British",
        "Other.ethnic.groups..Arab..Arab.Scottish.or.Arab.British",
        "Other.ethnic.groups..Other.ethnic.group",
        "White..Gypsy.Traveller",
        "White..Polish",
        "White"
    )
    
    
    c_long2 <- c_long2[!rows.to.drop,]
    
    
    dlply(c_long2, .(demog, year), f1)
    
    
    
    # finally, country of origion
    c_long2$variable <- car::recode(
        c_long2$variable,
        recodes="
        c('Elsewhere', 'Other.countries') = 'elsewhere';
        c('England') = 'england';
        c('Northern_Ireland', 'Northern.Ireland') = 'northern_ireland';
        c('White_Irish', 'White..Irish') = 'white.irish';
        c(
        'Other_EU',
        'Other.EU..Accession.countries.April.2001.to.March.2011',
        'Other.EU..Member.countries.in.March.2001..1.'
        ) = 'rest_of_eu';
        c(
        'Rep_Ireland',
        'Republic.of.Ireland'
        ) = 'rep_ireland';
        'Scotland' = 'scotland';
        'Wales' = 'wales'
        "
    )
    
    # Now, where there are multiple entries need to sum values
    c_long3 <- cast(c_long2, intermed + year + demog + variable ~ . , sum)
    
    c_long4 <- gdata::rename.vars(c_long3, from='(all)', to='value')
    
    output <- c_long4
    
    return(output)
}


Make_Tidy_Pathos <- function(){
    ####################
    # Now to tidy pathos data too
    
    m1 <- Pathos_Data
    
    m2 <- gdata::remove.vars(
        m1,
        names="X_merge"
    )
    
    m3 <- gdata::rename.vars(
        m2, 
        from=c(
            "Type_I_p_all_CY2001",
            "Type_II_p_all_CY2001",
            "Type_III_p_all_CY2001",
            "Type_IV_p_all_CY2001",
            "Type_All_p_all_CY2001",
            "Type_CORE_p_all_CY2001",
            "n_CY2001",
            "Type_I_p_all_CY2011",
            "Type_II_p_all_CY2011",
            "Type_III_p_all_CY2011",
            "Type_IV_p_all_CY2011",
            "Type_All_p_all_CY2011",
            "Type_CORE_p_all_CY2011",
            "n_CY2011"
        ),
        to=c(
            "i_2001",
            "ii_2001",
            "iii_2001",
            "iv_2001",
            "all_2001",
            "core_2001",
            "n_2001",
            "i_2011",
            "ii_2011",
            "iii_2011",
            "iv_2011",
            "all_2011",
            "core_2011",
            "n_2011"
        )                
    )
    
    m4 <- melt(
        m3,
        id.vars="intermed"
    )
    
    m5 <- cbind(
        m4, 
        colsplit(
            m4$variable,
            split="_",
            names=c("pathtype", "year")
        )
    )
    
    m6 <- gdata::remove.vars(
        m5,
        "variable"
    )
    
    m7 <- cast(
        m6,
        intermed + year ~ pathtype
    )
    
    output <- m7
    
    return(output)
}
