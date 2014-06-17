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
require(ellipse)
require(gdata)
require(car)



# Flags

Complete_Only <- T # Should only complete data be used? 
Network_Analysis <- F
Pathos_Analysis <- T
Areal_Unit_Conversion <- F
Aggregate_Census_OA <- T
Use_Census_Variables <- T
Census_Variables <- c(
    "Religion",
    "Ethnicity",
    "Country_Of_Origin"
    )

# Source files

source("Scripts/Functions.R")

source("Scripts/Load_and_Prepare_Data.R")



if (Network_Analysis){
    source("Scripts/Create_Networks.R")
    source("Scripts/Analyse_Networks.R")
}

source("Scripts/Make_Census_Pathos_Data_Tidy.R")


################################################################################
###### Work with pathos_tidy and census_tidy####################################
################################################################################

pathos_missing <- pathos_tidy
pathos_missing[,c("all", "core", "i", "ii", "iii", "iv")] <- apply(
    pathos_missing[,c("all", "core", "i", "ii" ,"iii", "iv")], 
    2, 
    function(x) {
        out <- x
        out[x==0] <- NA
        return(out)
    }
    )

pathos_missing_diffs <- ddply(
    pathos_missing,
    .(intermed),
    summarise,
    all=all[year==2011] - all[year==2001],
    core=core[year==2011] - core[year==2001],
    i=i[year==2011] - i[year==2001],
    ii=ii[year==2011] - ii[year==2001],
    iii=iii[year==2011] - iii[year==2001],
    iv = iv[year==2011] - iv[year==2001],
    n = n[year==2001] + n[year==2011]
    )

pathos_missing_diffs <- data.frame(intermed=pathos_missing_diffs[,1], year="diff", pathos_missing_diffs[,-1])


########### FIRST IMPORTANT VARIABLE TO MERGE
pathos_missing <- rbind(pathos_missing, pathos_missing_diffs)

## The following code estimates proportions in 2001 and 2011 
# of each ethnicity, religion and country of origin

f1 <- function(x){
    x <- remove.vars(x, names=c("year", "demog"), info=F)
    dwide  <- cast(x, intermed ~ variable)
    totals <- dwide$total
    intermed <- dwide$intermed
    valmat <- remove.vars(dwide, names=c("total", "intermed"), info=F)
    out <- sweep(valmat, 1, totals, "/")
    out <- data.frame(intermed=intermed, out)
    return(out)
}


census_demos <- dlply(
    census_tidy,
    .(demog, year),
    f1
    )



# I want to merge each of the objects in census_demos with pathos_missing_diffs

f2 <- function(x){
    out <- merge(
        x=x,
        y=pathos_missing_diffs,
        by="intermed", 
        all.y=T,
        all.x=F
        )
    return(out)
}

census_demos_merged <- llply(
    census_demos,
    f2
    )


# do the above, but only keep rows with observations > 10? 
f3 <- function(x){
    out <- x[x$n >= 10,]
    return(out)
}

census_demos_merged_n <- llply(
    census_demos_merged,
    f3
    )


###
f5 <- function(x){
    x <- remove.vars(x, names=c("year"), info=F)
    dwide  <- cast(x, intermed ~ variable)
    totals <- dwide$total
    intermed <- dwide$intermed
    valmat <- remove.vars(dwide, names=c("total", "intermed"), info=F)
    out <- sweep(valmat, 1, totals, "/")
    out <- data.frame(intermed=intermed, out)
    return(out)
}


census_demos <- dlply(
    census_tidy,
    .(demog, year),
    f1
)

## Want to look at differences in demography from 2001 to 2011
# using data in long format
# by intermed and variable
# find 2011 value and 2001 value
# if either is NA, return NA
# otherwise return difference

# First I want the proportion data to be in long format again


names_cd.jj <- names(census_demos)

census_p_long <- ldply(
    census_demos,
    melt,
    id="intermed"
    )
    
f6 <- function(x){
    if (dim(x)[1]==2){
        v2 <- x$value[x$year==2011]
        v1 <- x$value[x$year==2001]
        
        vd <- v2 - v1
        
        df <- x[1,]
        df$year <- "diff"
        df$value <- vd   
        out <- df
    } else {
        out <- NULL
    }
    return(out)
}



census_p_diff_long <- ddply(
    census_p_long,
    .(demog, intermed, variable),
    f6
    )

#### Second important variable to merge
census_p_all_long <- rbind(census_p_long, census_p_diff_long)


pathos_demos <- merge(
    x=census_p_all_long,
    y=pathos_missing,
    by=union("intermed", "year"),
    all.y=T,
    all.x=F
    )

# Now to remove rows where the values are < 10 

pathos_demos_bign <- pathos_demos[pathos_demos$n >= 10,]

pathos_demos.list_by_year <- dlply(
    pathos_demos_bign,
    .(year)
    )

##################################################################################################
pathos_demos_difs <- subset(
    pathos_demos_bign,
    subset=year=="diff"
    )
# remove missing values

is.missing <- is.na(pathos_demos_difs$value)

pathos_demos_difs <- pathos_demos_difs[!is.missing,]

# Split this up by demographic type

pathos_demos_difs_list <- dlply(pathos_demos_difs,
                                .(demog)
                                )

##################################################################################################
pathos_demos_difs <- subset(
    pathos_demos_bign,
    subset=year=="diff"
)
# remove missing values
is.missing <- is.na(pathos_demos_difs$value)
pathos_demos_difs <- pathos_demos_difs[!is.missing,]
head(pathos_demos_difs)
dim(pathos_demos_difs)
pathos_demos_difs_list <- dlply(
    pathos_demos_difs,
    .(demog)
)

pathos_demos_difs_list[[1]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]

png("Figures/Corrgram_d_rel_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between changes in religion and changes in pathos",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
    )

dev.off()

####
pathos_demos_difs_list[[2]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]


png("Figures/Corrgram_d_eth_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between changes in ethnicity and changes in pathos",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)
dev.off()

##################

pathos_demos_difs_list[[3]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]


png("Figures/Corrgram_d_coo_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between changes in country of origin and changes in pathos",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)
dev.off()


##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
pathos_demos_difs <- subset(
    pathos_demos_bign,
    subset=year=="2001"
)
# remove missing values
is.missing <- is.na(pathos_demos_difs$value)
pathos_demos_difs <- pathos_demos_difs[!is.missing,]
head(pathos_demos_difs)
dim(pathos_demos_difs)
pathos_demos_difs_list <- dlply(
    pathos_demos_difs,
    .(demog)
)

pathos_demos_difs_list[[1]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]

png("Figures/Corrgram_t1_rel_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between religion and pathos in 2001",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)

dev.off()

####
pathos_demos_difs_list[[2]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]


png("Figures/Corrgram_t1_eth_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between ethnicity and pathos in 2001",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)
dev.off()

##################

pathos_demos_difs_list[[3]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]


png("Figures/Corrgram_t1_coo_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations country of origin and pathos in 2001",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)
dev.off()

##################################################################################################
pathos_demos_difs <- subset(
    pathos_demos_bign,
    subset=year=="2011"
)
# remove missing values
is.missing <- is.na(pathos_demos_difs$value)
pathos_demos_difs <- pathos_demos_difs[!is.missing,]
head(pathos_demos_difs)
dim(pathos_demos_difs)
pathos_demos_difs_list <- dlply(
    pathos_demos_difs,
    .(demog)
)

pathos_demos_difs_list[[1]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]

png("Figures/Corrgram_t2_rel_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between religion and pathos in 2011",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)

dev.off()

####
pathos_demos_difs_list[[2]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]


png("Figures/Corrgram_t2_eth_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations between ethnicity and pathos in 2011",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)
dev.off()

##################

pathos_demos_difs_list[[3]] -> tmp
tmp2 <- cast(tmp,  ~ variable)
tmp1 <- tmp[,c("intermed", "all", "core", "i", "ii", "iii", "iv", "n")]
tmp2 <- tmp[, c("intermed", "variable", "value")]
tmp2a <- cast(tmp2, intermed ~ variable)
tmp3 <- merge(tmp2a, tmp1, by="intermed")
tmp4 <- tmp3[!duplicated(tmp3),]


png("Figures/Corrgram_t2_coo_d_path.png", width=2000, height=1500)
corrgram(
    tmp4[,-c(1, length(names(tmp4)))],
    upper.panel=panel.conf,
    main="\n\nCorrelations country of origin and pathos in 2011",
    col.regions=colorRampPalette(rev(c("red", "salmon", "white", "royalblue", "navy")))
)
dev.off()
