###############################################################################
# Load Country of Origin Data: merge OA to Intermediate geography

# Stage 1: Merge 
#   Census_2011__KS204SC__Country_Of_Origin
#       with
#   Census_2011_Lookup__OA_TO_HIGHER_AREAS


## Stages for each variable

# 1 ) Merge 2001 and 2011 tables independently to Pathos_Raw
# 2 ) Drop unused variables
# 3 ) rename 2001 and 2011 variables for consistently
# 4 ) merge 2001 and 2011 tables
# 5 ) create 'dif' variables

# On branch aggmerge: want to approach merging a different way, with aggregating at an earlier stage

# What data are really needed?

# OA_t1 to demographic [A]
# OA_t2 to demographic [B]

# Pathos to Inter [C]
# Inter to OA_t2 [D]
# OA_t1 to OA_t2 [E]

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

path_tidy <- m7

# path_tiday and c_long should be all that's needed
# merge using intersection of intermed and year

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

census_tidied <- c_long4

save(
    census_tidied,
    path_tidy,
    file="Data/RObj/Tidied_Data.RData"
    )

