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




if ("Country_Of_Origin" %in% Census_Variables){
    print("Finding or reconstructing Country_of_Origin merged file")

    if (!file.exists("Data/RObj/Pathos_Origin.RData")){
        print("Cannot find as RObj. Searching for CSV")
        if (!file.exists("Data/Raw/Pathos_Origin.csv")){
            print("Cannot find as csv. Creating from other data sources")
            
            # 1 ) Merge 2001 and 2011 tables independently to Pathos_Raw [DONE]
            # 2 ) Drop unused variables
            # 3 ) rename 2001 and 2011 variables for consistently
            # 4 ) merge 2001 and 2011 tables
            # 5 ) create 'dif' variables
            
            #########################################################################################################
            debug(Long_Merge)
            
            Pathos_COO.2001 <- Long_Merge(
                Origin= list(
                    DF=Pathos_Data,
                    Link.out="intermed"
                    ),
                Links=list(
                        A=list(
                            DF=Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                            Link.in="IntermediateZone2001Code",
                            Link.out="OutputArea2001Code"
                            ),

                        B=list(
                            DF=Census_2001_OA_Lookup,
                            Link.in="OutputArea2001Code",
                            Link.out="NRSoldOutputArea2001Code"
                        )
                    ),
                Target=list(
                    DF=Census_2001__KS005__Country_Of_Origin,
                    Link.in="Zone.Code"
                )
            )
            
            Pathos_COO.2011 <- Long_Merge(
                Origin= list(
                    DF=Pathos_Data,
                    Link.out="intermed"
                ),
                Links=list(
                    A=list(
                        DF=Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                        Link.in="IntermediateZone2001Code",
                        Link.out="OutputArea2011Code"
                    )
                ),
                Target=list(
                    DF=Census_2011__KS204SC__Country_Of_Origin,
                    Link.in="x1"
                )
            )
            
            
            # 2 ) Drop unused variables
            
            Pathos_COO.2001 <- subset(
                Pathos_COO.2001,
                select=c(
                    "intermed",
                    "Type_I_p_all_CY2001",

                    "Type_II_p_all_CY2001",
                    "Type_III_p_all_CY2001",
                    "Type_IV_p_all_CY2001",
                    "Type_All_p_all_CY2001",
                    "Type_CORE_p_all_CY2001",
                    "n_CY2001",
                    "All_People",
                    "England",
                    "Scotland",
                    "Wales",
                    "Northern_Ireland",
                    "Rep_Ireland",

                    "Other_EU",
                    "Elsewhere"
                    )
                )
            
            Pathos_COO.2001 <- plyr::rename(
                Pathos_COO.2001,
                replace=c(
                    "Type_I_p_all_CY2001"="pathos_i",
                    "Type_II_p_all_CY2001"="pathos_ii",
                    "Type_III_p_all_CY2001"="pathos_iii",

                    "Type_IV_p_all_CY2001" ="pathos_iv",
                    "Type_All_p_all_CY2001"="pathos_all",
                    "Type_CORE_p_all_CY2001"="pathos_core",
                    "n_CY2001"="pathos.n",

                    "All_People"="all_people",
                    "England"="england",
                    "Northern_Ireland"="northern_ireland",
                    "Scotland"="scotland",
                    "Rep_Ireland"="republic_of_ireland",
                    "Other_EU"="other_eu",
                    "Elsewhere"="elsewhere"
                    )
                )

            Pathos_COO.2001 <- data.frame(Pathos_COO.2001, year=2001)
                
            ###########
            Pathos_COO.2011 <- subset(
                Pathos_COO.2011,
                select=c(
                    "intermed",
                    "Type_I_p_all_CY2011",
                    "Type_II_p_all_CY2011",
                    "Type_III_p_all_CY2011",
                    "Type_IV_p_all_CY2011",
                    "Type_All_p_all_CY2011",
                    "Type_CORE_p_all_CY2011",
                    "n_CY2011",
                    "All.people",
                    "England",
                    "Northern.Ireland",
                    "Scotland",
                    "Wales",
                    "Republic.of.Ireland",
                    "Other.EU..Member.countries.in.March.2001..1.",
                    "Other.EU..Accession.countries.April.2001.to.March.2011",
                    "Other.countries"
                    )
                )
            
            Pathos_COO.2011 <- subset(
                Pathos_COO.2011, 
                replace=c(
                    "Type_I_p_all_CY2011" = "pathos_i",
                    "Type_II_p_all_CY2011" = "pathos_ii",
                    "Type_III_p_all_CY2011" = "pathos_iii",
                    "Type_IV_p_all_CY2011" = "pathos_iv",
                    "Type_All_p_all_CY2011" = "pathos_all",
                    "Type_CORE_p_all_CY2011" = "pathos_core",
                    "n_CY2011" = "pathos_n",
                    "All_People" = "all_people",
                    "England" = "england",
                    "Northern.Ireland" = "northern_ireland",
                    "Scotland" = "scotland",
                    "Republic.of.Ireland" = "republic_of_ireland",
                    "Wales" = "wales",
                    "Other.countries" = "elsewhere"
                    )
                )
            
            Pathos_COO.2011 <- plyr::mutate(
                Pathos_COO.2011, 
                other_eu = Other.EU..Member.countries.in.March.2001..1. +
                Other.EU..Accession.countries.April.2001.to.March.2011
                )
            
            Pathos_COO.2011 <- gdata::remove.vars(
                Pathos_COO.2011,
                names=c(
                    "Other.EU..Member.countries.in.March.2001..1.",
                    "Other.EU..Accession.countries.April.2001.to.March.2011"
                    )
                )
            
            Pathos_COO.2011 <- data.frame(
                Pathos_COO.2011, year=2011
                )
 
            

            # 3 ) rename 2001 and 2011 variables for consistently
            # 4 ) merge 2001 and 2011 tables
            # 5 ) create 'dif' variables
            

        
            ###############################
            
            merged.2001.backup <- merged.2001
            
            merged.2001 <- plyr::rename(
                merged.2001,
                replace=c(
                    "All_People"="Total",
                    "Rep_Ireland"= "Republic_of_Ireland"
                )
            )
            
            merged.2001.backup <- merged.2001
            
            merged.2001 <- subset(
                merged.2001,
                select=c(
                    "intermed",
                    "year", 
                    "variable",
                    "value",
                    "Total",
                    "England",
                    "Scotland",
                    "Wales",
                    "Northern_Ireland",
                    "Republic_of_Ireland",
                    "Other_EU",
                    "Elsewhere"
                )    
            )
            
            rm(merged.2001.backup)
            
            ##########################################################################
            ##########################################################################
            
#             
#             
#             merged.2011.backup <- merged.2011
#             
#             merged.2011$Other_EU <- (
#                 merged.2011$Other.EU..Member.countries.in.March.2001..1. + 
#                     merged.2011$Other.EU..Accession.countries.April.2001.to.March.2011
#             )
#             
#             merged.2011.backup <- merged.2011
#             
#             merged.2011 <- plyr::rename(
#                 merged.2011,
#                 replace=c(
#                     "IntermediateZone2001Code" = "intermed",
#                     "All.people" = "Total",
#                     "Northern.Ireland" = "Northern_Ireland",
#                     "Republic.of.Ireland" = "Republic_of_Ireland",
#                     "Other.countries" = "Elsewhere"
#                 )
#             )
#             
#             merged.2011.backup <- merged.2011
#             
#             merged.2011 <- subset(
#                 merged.2011,
#                 select=c(
#                     "intermed",
#                     "year", 
#                     "variable",
#                     "value",
#                     "Total",
#                     "England",
#                     "Scotland",
#                     "Wales",
#                     "Northern_Ireland",
#                     "Republic_of_Ireland",
#                     "Other_EU",
#                     "Elsewhere"      
#                 )
#             )
#             
#             derived.dif <- subset(
#                 Data_Long,
#                 year=="dif"
#             )
#             #####################################################
#             ####### Need to sum groups by intermed
#             
#             merged.2001 <- plyr::ddply(
#                 .data=merged.2001,
#                 .(intermed, year, variable, value),
#                 summarize,
#                 Total=sum(Total),
#                 England=sum(England),
#                 Scotland=sum(Scotland),
#                 Wales=sum(Wales),
#                 Northern_Ireland=sum(Northern_Ireland),
#                 Republic_of_Ireland=sum(Republic_of_Ireland),
#                 Other_EU=sum(Other_EU),
#                 Elsewhere=sum(Elsewhere)
#             )
#             
#             merged.2011 <- plyr::ddply(
#                 .data=merged.2011,
#                 .(intermed, year, variable, value),
#                 summarize,
#                 Total=sum(Total),
#                 England=sum(England),
#                 Scotland=sum(Scotland),
#                 Wales=sum(Wales),
#                 Northern_Ireland=sum(Northern_Ireland),
#                 Republic_of_Ireland=sum(Republic_of_Ireland),
#                 Other_EU=sum(Other_EU),
#                 Elsewhere=sum(Elsewhere)
#             )
#             
#             merged.both_years <- rbind(
#                 merged.2001,
#                 merged.2011
#             )
#             
#             
#             tmp <- plyr::ddply(
#                 merged.both_years,
#                 .(intermed, variable, value),
#                 summarise,
#                 Total                   =   Total[year==2011] - Total[year==2001],
#                 England                 =   England[year==2011] - England[year==2001],
#                 Scotland                =   Scotland[year==2011] - Scotland[year==2001],
#                 Wales                   =   Wales[year==2011] - Wales[year==2001],
#                 Northern_Ireland        =   Northern_Ireland[year==2011] - Northern_Ireland[year==2001],
#                 Republic_of_Ireland     =   Republic_of_Ireland[year==2011] - Republic_of_Ireland[year==2001],
#                 Other_EU                =   Other_EU[year==2011] - Other_EU[year==2001],
#                 Elsewhere               =   Elsewhere[year==2011] - Elsewhere[year==2001]
#             )
#             
#             tmp2 <- tmp[!duplicated(tmp$intermed),]
#             tmp3 <- tmp2[,c(1, 4:11)]
#             
#             tmp4 <- merge(
#                 x=derived.dif,
#                 y=tmp3,
#                 by="intermed",
#                 all=F
#             )
#             
#             merged.dif <- tmp4
#             
#             merged.all <- rbind(
#                 merged.both_years,
#                 merged.dif
#             )
#             
#             proportion_Scottish <- with(merged.all, Scotland/Total)
#             proportion_Scottish[merged.all$year=="dif"] <- NA
#             merged.all$proportion_Scottish <- proportion_Scottish
#             
#             proportion_English <- with(merged.all, England/Total)
#             proportion_English[merged.all$year=="dif"] <- NA
#             merged.all$proportion_English <- proportion_English
#             
#             proportion_Other_EU <- with(merged.all, Other_EU/Total)
#             proportion_Other_EU[merged.all$year=="dif"] <- NA
#             merged.all$proportion_Other_EU <- proportion_Other_EU
#             
#             proportion_Elsewhere <- with(merged.all, Elsewhere/Total)
#             proportion_Elsewhere[merged.all$year=="dif"] <- NA
#             merged.all$proportion_Elsewhere <- proportion_Elsewhere
#             
#             
#             
#             # Proportionate change in population
#             
#             # for each intermed
#             # total change / total in 2001
#             tmp <- plyr::ddply(
#                 merged.all,
#                 .(intermed, variable),
#                 summarise,
#                 proportion_pop_change = Total[year=="dif"]/Total[year=="2001"]
#             )
#             
#             tmp2 <- data.frame(tmp, year="dif")
#             
#             
#             tmp3 <- merge(
#                 x=merged.all,
#                 y=tmp2, 
#                 by=union(
#                     "intermed",
#                     "year"
#                 ),
#                 all=F
#             )
#             
#             tmp4 <- gdata::remove.vars(
#                 data=tmp3,
#                 names="variable.y"
#             )
#             
#             tmp5 <- plyr::rename(
#                 x=tmp4,
#                 replace=c(
#                     "variable.x" = "variable"
#                 )
#             )
#             
#             
#             # Use the variables proportion scottish, proportion english, proportion etc to show difference 
#             # in proportions in dif years
#             
#             tmp6 <- plyr::ddply(
#                 tmp5,
#                 .(intermed, variable),
#                 summarise,
#                 p_Scottish = proportion_Scottish[year==2011] - proportion_Scottish[year==2001],
#                 p_English = proportion_English[year==2011] - proportion_English[year==2001],
#                 p_Other_EU = proportion_Other_EU[year==2011] - proportion_Other_EU[year==2001],
#                 p_Elsewhere = proportion_Elsewhere[year==2011] - proportion_Elsewhere[year==2001]
#             )
#             
#             tmp7 <- gdata::remove.vars(
#                 tmp6,
#                 names="variable"
#             )
#             
#             tmp8 <- merge(
#                 x=merged.all,
#                 y=tmp7,
#                 by="intermed",
#                 all=F
#             )
#             
#             # Copy over p_scottish etc where year is "dif"
#             tmp8[
#                 tmp8$year=="dif",
#                 c(
#                     "proportion_Scottish",
#                     "proportion_English",
#                     "proportion_Other_EU",
#                     "proportion_Elsewhere"
#                 )
#                 ] <- tmp8[tmp8$year=="dif",
#                           c(
#                               "p_Scottish",
#                               "p_English",
#                               "p_Other_EU",
#                               "p_Elsewhere"
#                           )
#                           ]
#             
#             tmp9 <- gdata::remove.vars(
#                 tmp8,
#                 names=c(
#                     "p_Scottish",
#                     "p_English",
#                     "p_Other_EU",
#                     "p_Elsewhere"
#                 )
#             )
#             
#             tmp10 <- tmp9[!duplicated(tmp9),]
#             
#             
#             Pathos_Demos <- tmp10
            
            rm(list=ls(pattern="tmp*"))
            rm(list=ls(pattern="^x"))
            rm(list=ls(pattern="^y"))
            rm(list=ls(pattern="^merged"))
            
            
            write.csv(
                Pathos_Demos,
                file="Data/Raw/Pathos_Origin.csv"
            )
            
            save(
                Pathos_Demos,
                file="Data/RObj/Pathos_Origin.RData"
            )
        } else {
            print("Found as csv. Loading")
            Pathos_Origin <- read.csv("Data/Raw/Pathos_Origin.csv")
            save(
                Pathos_Origin,
                file="Data/RObj/Pathos_Origin.RData"
            )
        }   
    } else {
        print("Found as RData object. Loading")
        load("Data/RObj/Pathos_Origin.RData")
        if (!file.exists("Data/Raw/Pathos_Origin.csv")){
            write.csv(
                Pathos_Origin,
                file="Data/Raw/Pathos_Origin.csv"
            )
            
        }
    }
    
    
    
}

if ("Ethnicity" %in% Census_Variables){
    if (!file.exists("Data/RObj/Pathos_Ethnicity.RData")){
        print("Cannot find as RObj. Searching for CSV")
        if (!file.exists("Data/Raw/Pathos_Ethnicity.csv")){
            print("Cannot find as csv. Creating from other data sources")
            
<<<<<<< HEAD
            Pathos_Eth.2001 <- Long_Merge(
                Origin=list(
                    DF=Pathos_Data,
                    Link.out="intermed"
                    ),
                
                Links=list(
                    A=list(
                        DF=Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                        Link.in="IntermediateZone2001Code",
                        Link.out="OutputArea2001Code"
                        ),
                    B=list(
                        DF=Census_2001_OA_Lookup,
                        Link.in="OutputArea2001Code",
                        Link.out="NRSoldOutputArea2001Code"
                        )
                    ),
                
                Target=list(
                    DF=Census_2001__KS006__Ethnicity,
                    Link.in="Zone.Code"
                    )
                )
            

            Pathos_Eth.2011 <- Long_Merge(

                Origin=list(
                    DF=Pathos_Data,
                    Link.out="intermed"
                    ),
                
                Links=list(
                    A=list(
                        DF=Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                        Link.in="IntermediateZone2001Code",

                        Link.out="OutputArea2011Code"
                    )
                ),
                
                Target=list(
                    DF=Census_2011__KS201SC__Ethnicity,
                    Link.in="x1"
                    )
                
                )

            
            ######################################################
            ####### further kludging below to be continued #######
            ####################################################
            
            Pathos_Eth.2001 <- subset(
                Pathos_Eth.2001,
                select=c(
                    "intermed",                                   
                    "Type_I_p_all_CY2001",                       
                    "Type_II_p_all_CY2001",                       
                    "Type_III_p_all_CY2001",                     
                    "Type_IV_p_all_CY2001",                       
                    "Type_All_p_all_CY2001",                     
                    "Type_CORE_p_all_CY2001",                     
                    "n_CY2001",                                  
                    "All_People",                                 
                    "White_Scottish",                            
                    "Other_White_British",                        
                    "White_Irish",                               
                    "Other_White",                                
                    "Indian",                                    
                    "Pakistani",                                  
                    "Bangladeshi",                                             
                    "Chinese",                                   
                    "Caribbean",                                  
                    "African",                                   
                    "Black_Scottish_Or_Other_Black",              
                    "Any_Mixed_Background",                      
                    "Other_Ethnic_Group"
                    )
                )
            
            Pathos_Eth.2001 <- plyr::mutate(
                Pathos_Eth.2001,
                black_afrocaribbean = African + Black_Scottish_Or_Other_Black + Caribbean
                )
            
             Pathos_Eth.2001 <- plyr::rename(
                 Pathos_Eth.2001,
                 replace=c(
                     "Type_I_p_all_CY2001"="pathos_i",                       
                     "Type_II_p_all_CY2001"= "pathos_ii",                       
                     "Type_III_p_all_CY2001"= "pathos_iii",                     
                     "Type_IV_p_all_CY2001"="pathos_iv",                       
                     "Type_All_p_all_CY2001"= "pathos_all",                     
                     "Type_CORE_p_all_CY2001"="pathos_core",                     
                     "n_CY2001"="pathos_n",                                  
                     "All_People"="all_people",                                 
                     "White_Scottish"="white_scottish",                            
                     "Other_White_British"="white_other_british",                        
                     "White_Irish"="white_irish",                               
                     "Other_White"="white_other",                                
                     "Indian"="indian",                                    
                     "Pakistani"="pakistani",                                  
                     "Bangladeshi"="bangladeshi",                                                     
                     "Chinese"="chinese",                                   
                     "Any_Mixed_Background"="any_mixed",                      
                     "Other_Ethnic_Group"="other_ethnic_group"
                     )
                 )
             #################
             
            Pathos_Eth.2001 <- gdata::remove.vars(
                Pathos_Eth.2001, 
                names=c(
                    "Other_South_Asian",
                    "Caribbean",
                    "Black_Scottish_Or_Other_Black"
                    ) 
                )
            
            
            
            
            Pathos_Eth.2011 <- subset(
                Pathos_Eth.2011,
                select=c(
                    "intermed",                                                                                        
                    "Type_I_p_all_CY2011",                                                                             
                    "Type_II_p_all_CY2011",                                                                            
                    "Type_III_p_all_CY2011",                                                                           
                    "Type_IV_p_all_CY2011",                                                                            
                    "Type_All_p_all_CY2011",                                                                           
                    "Type_CORE_p_all_CY2011",                                                                          
                    "n_CY2011",                                                                                       
                    
                    "All.people",                                                                                      
                    "White..Scottish",                                                                                 
                    "White..Other.British",                                                                            
                    "White..Irish",                                                                                    
                    "White..Gypsy.Traveller",                                                                          
                    "White..Polish",                                                                                   
                    "White..Other.White",                                                                              
                    "Mixed.or.multiple.ethnic.groups",                                                                 
                    
                    "Asian..Asian.Scottish.or.Asian.British..Pakistani..Pakistani.Scottish.or.Pakistani.British",      
                    "Asian..Asian.Scottish.or.Asian.British..Indian..Indian.Scottish.or.Indian.British",               
                    "Asian..Asian.Scottish.or.Asian.British..Bangladeshi..Bangladeshi.Scottish.or.Bangladeshi.British",
                    "Asian..Asian.Scottish.or.Asian.British..Chinese..Chinese.Scottish.or.Chinese.British",            
                    "African",                                                                                         
                    "Caribbean.or.Black",                                                                              
                    "Other.ethnic.groups"                                                                             
                    )
                )
            
            Pathos_Eth.2011 <- plyr::mutate(
                Pathos_Eth.2011,
                black_afrocaribbean = African + Caribbean.or.Black,
                white_other = White..Gypsy.Traveller + White..Polish + White..Other.White
            )
            
            
            Pathos_Eth.2011 <- plyr::rename(
                Pathos_Eth.2011,
                replace=c(
                    "Type_I_p_all_CY2011"="pathos_i",                                                                             
                    "Type_II_p_all_CY2011"="pathos_ii",                                                                            
                    "Type_III_p_all_CY2011"="pathos_iii",                                                                           
                    "Type_IV_p_all_CY2011"="pathos_iv",                                                                            
                    "Type_All_p_all_CY2011"="pathos_all",                                                                           
                    "Type_CORE_p_all_CY2011"="pathos_core",                                                                          
                    "n_CY2011"="pathos_n",                                                                                       
                    
                    "All.people"="all_people",                                                                                      
                    "White..Scottish"="white_scottish",                                                                                 
                    "White..Other.British"="white_other_british",                                                                            
                    "White..Irish"="white_irish",                                                                      
                    "Mixed.or.multiple.ethnic.groups"="any_mixed",                                                                 
                    
                    "Asian..Asian.Scottish.or.Asian.British..Pakistani..Pakistani.Scottish.or.Pakistani.British"="pakistani",      
                    "Asian..Asian.Scottish.or.Asian.British..Indian..Indian.Scottish.or.Indian.British"="indian",               
                    "Asian..Asian.Scottish.or.Asian.British..Bangladeshi..Bangladeshi.Scottish.or.Bangladeshi.British"="bangladeshi",
                    "Asian..Asian.Scottish.or.Asian.British..Chinese..Chinese.Scottish.or.Chinese.British"="chinese",            
                    "Other.ethnic.groups"="other_ethnic_group"                                                                             
                )    
            )
            
            Pathos_Eth.2011 <- gdata::remove.vars(
                Pathos_Eth.2011,
                names=c(
                    "African",
                    "Caribbean.or.Black",
                    "White..Gypsy.Traveller",
                    "White..Polish",
                    "White..Other.White"
                    )
            )
            
            Pathos_Eth.2001 <- cbind(Pathos_Eth.2001, year=2001)
            Pathos_Eth.2011 <- cbind(Pathos_Eth.2011, year=2011)

            
            
            #####################################################
            ####### Need to sum groups by intermed
            tmp <- plyr::ddply(
                .data=Pathos_Eth.2001,
                .(intermed),
                summarize,
                all_people=sum(all_people), 
                white_scottish=sum(white_scottish),
                white_other_british=sum(white_other_british),
                white_irish=sum(white_irish),
                white_other=sum(white_other),
                indian=sum(indian),
                pakistani=sum(pakistani),
                bangladeshi=sum(bangladeshi),
                chinese=sum(chinese),
                any_mixed=sum(any_mixed),
                other_ethnic_group=sum(other_ethnic_group),
                black_afrocaribbean=sum(black_afrocaribbean)
            )
            pathos.vars <- c(
                "pathos_i", "pathos_ii", "pathos_iii", "pathos_iv", 
                "pathos_all", "pathos_core"
            )
            tmp2 <- Pathos_Eth.2001[,c("intermed", pathos.vars, "year")]
            tmp2 <- tmp2[!duplicated(tmp2),]
            
            Pathos_Eth.2001 <- merge(
                tmp, tmp2,
                by="intermed",
                all.x=T,
                all.y=F
                )
            
            ####### Need to sum groups by intermed
            tmp <- plyr::ddply(
                .data=Pathos_Eth.2011,
                .(intermed),
                summarize,
                all_people=sum(all_people), 
                white_scottish=sum(white_scottish),
                white_other_british=sum(white_other_british),
                white_irish=sum(white_irish),
                white_other=sum(white_other),
                indian=sum(indian),
                pakistani=sum(pakistani),
                bangladeshi=sum(bangladeshi),
                chinese=sum(chinese),
                any_mixed=sum(any_mixed),
                other_ethnic_group=sum(other_ethnic_group),
                black_afrocaribbean=sum(black_afrocaribbean)
            )

            tmp2 <- Pathos_Eth.2011[,c("intermed", pathos.vars, "year")]
            tmp2 <- tmp2[!duplicated(tmp2),]
            
            Pathos_Eth.2011 <- merge(
                tmp, tmp2,
                by="intermed",
                all.x=T,
                all.y=F
            )
            
            
            
            
            
            merged.2001 <- plyr::ddply(
                .data=merged.2001,
                .(intermed, year, variable, value),
                summarize,
                Total=sum(Total),
                White_Scottish=sum(White_Scottish),
                Other_White_British=sum(Other_White_British),
                White_Irish=sum(White_Irish),
                Other_White=sum(Other_White),
                Indian=sum(Indian),
                Pakistani=sum(Pakistani),
                Bangladeshi=sum(Bangladeshi),
                Other_South_Asian=sum(Other_South_Asian),
                Chinese=sum(Chinese),
                Caribbean=sum(Caribbean)
            )
            
            merged.2011 <- plyr::ddply(
                .data=merged.2011,
                .(intermed, year, variable, value),
                summarize,
                Total=sum(Total),
                England=sum(England),
                Scotland=sum(Scotland),
                Wales=sum(Wales),
                Northern_Ireland=sum(Northern_Ireland),
                Republic_of_Ireland=sum(Republic_of_Ireland),
                Other_EU=sum(Other_EU),
                Elsewhere=sum(Elsewhere)
            )
            
            merged.both_years <- rbind(
                merged.2001,
                merged.2011
            )
            
            
            tmp <- plyr::ddply(
                merged.both_years,
                .(intermed, variable, value),
                summarise,
                Total                   =   Total[year==2011] - Total[year==2001],
                England                 =   England[year==2011] - England[year==2001],
                Scotland                =   Scotland[year==2011] - Scotland[year==2001],
                Wales                   =   Wales[year==2011] - Wales[year==2001],
                Northern_Ireland        =   Northern_Ireland[year==2011] - Northern_Ireland[year==2001],
                Republic_of_Ireland     =   Republic_of_Ireland[year==2011] - Republic_of_Ireland[year==2001],
                Other_EU                =   Other_EU[year==2011] - Other_EU[year==2001],
                Elsewhere               =   Elsewhere[year==2011] - Elsewhere[year==2001]
            )
            
            tmp2 <- tmp[!duplicated(tmp$intermed),]
            tmp3 <- tmp2[,c(1, 4:11)]
            
            tmp4 <- merge(
                x=derived.dif,
                y=tmp3,
                by="intermed",
                all=F
            )
            
            merged.dif <- tmp4
            
            merged.all <- rbind(
                merged.both_years,
                merged.dif
            )
            
            proportion_Scottish <- with(merged.all, Scotland/Total)
            proportion_Scottish[merged.all$year=="dif"] <- NA
            merged.all$proportion_Scottish <- proportion_Scottish
            
            proportion_English <- with(merged.all, England/Total)
            proportion_English[merged.all$year=="dif"] <- NA
            merged.all$proportion_English <- proportion_English
            
            proportion_Other_EU <- with(merged.all, Other_EU/Total)
            proportion_Other_EU[merged.all$year=="dif"] <- NA
            merged.all$proportion_Other_EU <- proportion_Other_EU
            
            proportion_Elsewhere <- with(merged.all, Elsewhere/Total)
            proportion_Elsewhere[merged.all$year=="dif"] <- NA
            merged.all$proportion_Elsewhere <- proportion_Elsewhere
            
#             
#             
#             # Proportionate change in population
#             
#             # for each intermed
#             # total change / total in 2001
#             tmp <- plyr::ddply(
#                 merged.all,
#                 .(intermed, variable),
#                 summarise,
#                 proportion_pop_change = Total[year=="dif"]/Total[year=="2001"]
#             )
#             
#             tmp2 <- data.frame(tmp, year="dif")
#             
#             
#             tmp3 <- merge(
#                 x=merged.all,
#                 y=tmp2, 
#                 by=union(
#                     "intermed",
#                     "year"
#                 ),
#                 all.x=T,
#                 all.y=F
#             )
#             
#             tmp4 <- gdata::remove.vars(
#                 data=tmp3,
#                 names="variable.y"
#             )
#             
#             tmp5 <- plyr::rename(
#                 x=tmp4,
#                 replace=c(
#                     "variable.x" = "variable"
#                 )
#             )
#             
#             
#             # Use the variables proportion scottish, proportion english, proportion etc to show difference 
#             # in proportions in dif years
#             
#             tmp6 <- plyr::ddply(
#                 tmp5,
#                 .(intermed, variable),
#                 summarise,
#                 p_Scottish = proportion_Scottish[year==2011] - proportion_Scottish[year==2001],
#                 p_English = proportion_English[year==2011] - proportion_English[year==2001],
#                 p_Other_EU = proportion_Other_EU[year==2011] - proportion_Other_EU[year==2001],
#                 p_Elsewhere = proportion_Elsewhere[year==2011] - proportion_Elsewhere[year==2001]
#             )
#             
#             tmp7 <- gdata::remove.vars(
#                 tmp6,
#                 names="variable"
#             )
#             
#             tmp8 <- merge(
#                 x=merged.all,
#                 y=tmp7,
#                 by="intermed",
#                 all.x=T,
#                 all.y=F
#             )
#             
#             # Copy over p_scottish etc where year is "dif"
#             tmp8[
#                 tmp8$year=="dif",
#                 c(
#                     "proportion_Scottish",
#                     "proportion_English",
#                     "proportion_Other_EU",
#                     "proportion_Elsewhere"
#                 )
#                 ] <- tmp8[tmp8$year=="dif",
#                           c(
#                               "p_Scottish",
#                               "p_English",
#                               "p_Other_EU",
#                               "p_Elsewhere"
#                           )
#                           ]
#             
#             tmp9 <- gdata::remove.vars(
#                 tmp8,
#                 names=c(
#                     "p_Scottish",
#                     "p_English",
#                     "p_Other_EU",
#                     "p_Elsewhere"
#                 )
#             )
#             
#             tmp10 <- tmp9[!duplicated(tmp9),]
#             
#             
#             Pathos_Demos <- tmp10
#             
#             rm(list=ls(pattern="tmp*"))
#             rm(list=ls(pattern="^x"))
#             rm(list=ls(pattern="^y"))
#             rm(list=ls(pattern="^merged"))
            
            
            write.csv(
                Pathos_Ethnicity,
                file="Data/Raw/Pathos_Ethnicity.csv"
            )
            
            save(
                Pathos_Ethnicity,
                file="Data/RObj/Pathos_Ethnicity.RData"
            )
        } else {
            print("Found as csv. Loading")
            Pathos_Ethnicity <- read.csv("Data/Raw/Pathos_Ethnicity.csv")
            save(
                Pathos_Ethnicity,
                file="Data/RObj/Pathos_Ethnicity.RData"
            )
        }   
    } else {
        print("Found as RData object. Loading")
        load("Data/RObj/Pathos_Ethnicity.RData")
        if (!file.exists("Data/Raw/Pathos_Ethnicity.csv")){
            write.csv(
                Pathos_Ethnicity,
                file="Data/Raw/Pathos_Ethnicity.csv"
            )
            
        }
    }
    
}


if ("Religion" %in% Census_Variables){
    if (!file.exists("Data/RObj/Pathos_Religion.RData")){
        print("Cannot find as RObj. Searching for CSV")
        if (!file.exists("Data/Raw/Pathos_Religion.csv")){
            print("Cannot find as csv. Creating from other data sources")
            
            Pathos_Religion.2001 <- Long_Merge(
                Origin=list(
                    DF=Pathos_Data,
                    Link.out="intermed"
                ),
                
                Links=list(
                    A=list(
                        DF=Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                        Link.in="IntermediateZone2001Code",
                        Link.out="OutputArea2001Code"
                    ),
                    B=list(
                        DF=Census_2001_OA_Lookup,
                        Link.in="OutputArea2001Code",
                        Link.out="NRSoldOutputArea2001Code"
                    )
                ),
                
                Target=list(
                    DF=Census_2001__KS007__Religion,
                    Link.in="Zone.Code"
                )
            )
            
            
            Pathos_Ethnicity.2011 <- Long_Merge(
                Origin=list(
                    DF=Pathos_Data,
                    Link.out="intermed"
                ),
                
                Links=list(
                    A=list(
                        DF=Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                        Link.in="IntermediateZone2001Code",
                        Link.out="OutputArea2011Code"
                    )
                ),
                
                Target=list(
                    DF=Census_2011__KS209SCb__Religion,
                    Link.in="x1"
                )
                
            )
            
            
            ###############################
            
            merged.2001.backup <- merged.2001
            
            merged.2001 <- plyr::rename(
                merged.2001,
                replace=c(
                    "All_People"="Total",
                    "Rep_Ireland"= "Republic_of_Ireland"
                )
            )
            
            merged.2001.backup <- merged.2001
            
            merged.2001 <- subset(
                merged.2001,
                select=c(
                    "intermed",
                    "year", 
                    "variable",
                    "value",
                    "Total",
                    "England",
                    "Scotland",
                    "Wales",
                    "Northern_Ireland",
                    "Republic_of_Ireland",
                    "Other_EU",
                    "Elsewhere"
                )    
            )
            
            rm(merged.2001.backup)
            
            #################
            
            
            
            merged.2011.backup <- merged.2011
            
            merged.2011$Other_EU <- (
                merged.2011$Other.EU..Member.countries.in.March.2001..1. + 
                    merged.2011$Other.EU..Accession.countries.April.2001.to.March.2011
            )
            
            merged.2011.backup <- merged.2011
            
            merged.2011 <- plyr::rename(
                merged.2011,
                replace=c(
                    "IntermediateZone2001Code" = "intermed",
                    "All.people" = "Total",
                    "Northern.Ireland" = "Northern_Ireland",
                    "Republic.of.Ireland" = "Republic_of_Ireland",
                    "Other.countries" = "Elsewhere"
                )
            )
            
            merged.2011.backup <- merged.2011
            
            merged.2011 <- subset(
                merged.2011,
                select=c(
                    "intermed",
                    "year", 
                    "variable",
                    "value",
                    "Total",
                    "England",
                    "Scotland",
                    "Wales",
                    "Northern_Ireland",
                    "Republic_of_Ireland",
                    "Other_EU",
                    "Elsewhere"      
                )
            )
            
            derived.dif <- subset(
                Data_Long,
                year=="dif"
            )
            #####################################################
            ####### Need to sum groups by intermed
            
            merged.2001 <- plyr::ddply(
                .data=merged.2001,
                .(intermed, year, variable, value),
                summarize,
                Total=sum(Total),
                England=sum(England),
                Scotland=sum(Scotland),
                Wales=sum(Wales),
                Northern_Ireland=sum(Northern_Ireland),
                Republic_of_Ireland=sum(Republic_of_Ireland),
                Other_EU=sum(Other_EU),
                Elsewhere=sum(Elsewhere)
            )
            
            merged.2011 <- plyr::ddply(
                .data=merged.2011,
                .(intermed, year, variable, value),
                summarize,
                Total=sum(Total),
                England=sum(England),
                Scotland=sum(Scotland),
                Wales=sum(Wales),
                Northern_Ireland=sum(Northern_Ireland),
                Republic_of_Ireland=sum(Republic_of_Ireland),
                Other_EU=sum(Other_EU),
                Elsewhere=sum(Elsewhere)
            )
            
            merged.both_years <- rbind(
                merged.2001,
                merged.2011
            )
            
            
            tmp <- plyr::ddply(
                merged.both_years,
                .(intermed, variable, value),
                summarise,
                Total                   =   Total[year==2011] - Total[year==2001],
                England                 =   England[year==2011] - England[year==2001],
                Scotland                =   Scotland[year==2011] - Scotland[year==2001],
                Wales                   =   Wales[year==2011] - Wales[year==2001],
                Northern_Ireland        =   Northern_Ireland[year==2011] - Northern_Ireland[year==2001],
                Republic_of_Ireland     =   Republic_of_Ireland[year==2011] - Republic_of_Ireland[year==2001],
                Other_EU                =   Other_EU[year==2011] - Other_EU[year==2001],
                Elsewhere               =   Elsewhere[year==2011] - Elsewhere[year==2001]
            )
            
            tmp2 <- tmp[!duplicated(tmp$intermed),]
            tmp3 <- tmp2[,c(1, 4:11)]
            
            tmp4 <- merge(
                x=derived.dif,
                y=tmp3,
                by="intermed",
                all=F
            )
            
            merged.dif <- tmp4
            
            merged.all <- rbind(
                merged.both_years,
                merged.dif
            )
            
            proportion_Scottish <- with(merged.all, Scotland/Total)
            proportion_Scottish[merged.all$year=="dif"] <- NA
            merged.all$proportion_Scottish <- proportion_Scottish
            
            proportion_English <- with(merged.all, England/Total)
            proportion_English[merged.all$year=="dif"] <- NA
            merged.all$proportion_English <- proportion_English
            
            proportion_Other_EU <- with(merged.all, Other_EU/Total)
            proportion_Other_EU[merged.all$year=="dif"] <- NA
            merged.all$proportion_Other_EU <- proportion_Other_EU
            
            proportion_Elsewhere <- with(merged.all, Elsewhere/Total)
            proportion_Elsewhere[merged.all$year=="dif"] <- NA
            merged.all$proportion_Elsewhere <- proportion_Elsewhere
            
            # Proportionate change in population
            
            # for each intermed
            # total change / total in 2001
            tmp <- plyr::ddply(
                merged.all,
                .(intermed, variable),
                summarise,
                proportion_pop_change = Total[year=="dif"]/Total[year=="2001"]
            )
            
            tmp2 <- data.frame(tmp, year="dif")
            
            
            tmp3 <- merge(
                x=merged.all,
                y=tmp2, 
                by=union(
                    "intermed",
                    "year"
                ),
                all.x=T,
                all.y=F
            )
            
            tmp4 <- gdata::remove.vars(
                data=tmp3,
                names="variable.y"
            )
            
            tmp5 <- plyr::rename(
                x=tmp4,
                replace=c(
                    "variable.x" = "variable"
                )
            )
            
            
            # Use the variables proportion scottish, proportion english, proportion etc to show difference 
            # in proportions in dif years
            
            tmp6 <- plyr::ddply(
                tmp5,
                .(intermed, variable),
                summarise,
                p_Scottish = proportion_Scottish[year==2011] - proportion_Scottish[year==2001],
                p_English = proportion_English[year==2011] - proportion_English[year==2001],
                p_Other_EU = proportion_Other_EU[year==2011] - proportion_Other_EU[year==2001],
                p_Elsewhere = proportion_Elsewhere[year==2011] - proportion_Elsewhere[year==2001]
            )
            
            tmp7 <- gdata::remove.vars(
                tmp6,
                names="variable"
            )
            
            tmp8 <- merge(
                x=merged.all,
                y=tmp7,
                by="intermed",
                all.x=T,
                all.y=F
            )
            
            # Copy over p_scottish etc where year is "dif"
            tmp8[
                tmp8$year=="dif",
                c(
                    "proportion_Scottish",
                    "proportion_English",
                    "proportion_Other_EU",
                    "proportion_Elsewhere"
                )
                ] <- tmp8[tmp8$year=="dif",
                          c(
                              "p_Scottish",
                              "p_English",
                              "p_Other_EU",
                              "p_Elsewhere"
                          )
                          ]
            
            tmp9 <- gdata::remove.vars(
                tmp8,
                names=c(
                    "p_Scottish",
                    "p_English",
                    "p_Other_EU",
                    "p_Elsewhere"
                )
            )
            
            tmp10 <- tmp9[!duplicated(tmp9),]
            
            
            Pathos_Demos <- tmp10
            
            rm(list=ls(pattern="tmp*"))
            rm(list=ls(pattern="^x"))
            rm(list=ls(pattern="^y"))
            rm(list=ls(pattern="^merged"))
            
            
            write.csv(
                Pathos_Demos,
                file="Data/Raw/Pathos_Demos.csv"
            )
            
            save(
                Pathos_Demos,
                file="Data/RObj/Pathos_Demos.RData"
            )
        } else {
            print("Found as csv. Loading")
            Pathos_Demos <- read.csv("Data/Raw/Pathos_Demos.csv")
            save(
                Pathos_Demos,
                file="Data/RObj/Pathos_Demos.RData"
            )
        }   
    } else {
        print("Found as RData object. Loading")
        load("Data/RObj/Pathos_Demos.RData")
        if (!file.exists("Data/Raw/Pathos_Demos.csv")){
            write.csv(
                Pathos_Demos,
                file="Data/Raw/Pathos_Demos.csv"
            )
            
        }
    }
    
}

}

