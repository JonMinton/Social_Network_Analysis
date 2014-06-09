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


