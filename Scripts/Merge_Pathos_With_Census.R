###############################################################################
# Load Country of Origin Data: merge OA to Intermediate geography

# Stage 1: Merge 
#   Census_2011__KS204SC__Country_Of_Origin
#       with
#   Census_2011_Lookup__OA_TO_HIGHER_AREAS

if ("Country_Of_Origin" %in% Census_Variables){
    if (!file.exists("Data/RObj/Pathos_Demos.RData")){
        print("Cannot find as RObj. Searching for CSV")
        if (!file.exists("Data/Raw/Pathos_Demos.csv")){
            print("Cannot find as csv. Creating from other data sources")
            
            y.ss <- subset(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                select=c(
                    "OutputArea2011Code",
                    "IntermediateZone2001Code",
                    "MasterPostcode"
                )
            )
            
            x1 <- Census_2011__KS204SC__Country_Of_Origin
            
            x2 <- merge(
                x=x1,
                y=y.ss,
                
                by.x="X",
                by.y="OutputArea2011Code",
                all=T
            )
            
            y.2011 <- subset(Data_Long, year==2011)
            
            x3 <- merge(
                x=x2,
                y=y.2011,
                by.x="IntermediateZone2001Code",
                by.y="intermed",
                all=T
            )
            
            merged.2011 <- x3
            
            merged.2011 <- merged.2011[!is.na(merged.2011$value),]
            
            
            # # Want to know proportion Non-Scottish
            # 
            # prop_nonScottish <- 1 - x4[,"Scotland"]/x4[,"All.people"]
            # 
            # plot(density(prop_nonScottish))
            # origin.deciles <- quantile(prop_nonScottish, 0:10/10)
            # 
            # p2 <- cut(prop_nonScottish, origin.deciles, include.lowest=T, labels=F)
            # head(p2)
            # 
            
            ################################################################################
            # Do the same with 2001 Country of Origin Data
            
            # Pathos_Data
            #  : intermed [A]
            
            y1 <- subset(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                select=c(
                    "OutputArea2011Code",
                    "OutputArea2001Code",
                    "IntermediateZone2001Code"
                )
            )
            
            x.2001 <- subset(Data_Long, year==2001)
            
            x2 <- merge(
                x=x.2001,
                y=y1,
                
                by.x="intermed",
                by.y="IntermediateZone2001Code",
                all=F
            )
            
            # Census_2011_Lookup_OA_TO_HIGHER_AREAS
            #  : OutputArea2011Code
            #  : OutputArea2001Code [B]
            #  : IntermediateZone2001Code [A]
            
            x3 <- merge(
                x=x2,
                y=Census_2001_OA_Lookup,
                
                by="OutputArea2001Code",
                all=F
            )
            
            
            # Census_2001_OA_Lookup
            #   : OutputArea2001Code [B]
            #   : NRSoldOutputArea2001Code [C]
            
            x4 <- merge(
                x=x3, 
                y=Census_2001__KS005__Country_Of_Origin,
                
                by.x="NRSoldOutputArea2001Code",
                by.y="Zone.Code",
                all=F
            )
            
            merged.2001 <- x4
            rm(x1, x2, x3, x4, x.2001, y.2011, y1, y.ss)
            
            
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

if ("Ethnicity" %in% Census_Variables){
    if (!file.exists("Data/RObj/Pathos_Demos.RData")){
        print("Cannot find as RObj. Searching for CSV")
        if (!file.exists("Data/Raw/Pathos_Demos.csv")){
            print("Cannot find as csv. Creating from other data sources")
            
            y.ss <- subset(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                select=c(
                    "OutputArea2011Code",
                    "IntermediateZone2001Code",
                    "MasterPostcode"
                )
            )
            
            x1 <- Census_2011__KS204SC__Country_Of_Origin
            
            x2 <- merge(
                x=x1,
                y=y.ss,
                
                by.x="X",
                by.y="OutputArea2011Code",
                all=T
            )
            
            y.2011 <- subset(Data_Long, year==2011)
            
            x3 <- merge(
                x=x2,
                y=y.2011,
                by.x="IntermediateZone2001Code",
                by.y="intermed",
                all=T
            )
            
            merged.2011 <- x3
            
            merged.2011 <- merged.2011[!is.na(merged.2011$value),]
            
            
            # # Want to know proportion Non-Scottish
            # 
            # prop_nonScottish <- 1 - x4[,"Scotland"]/x4[,"All.people"]
            # 
            # plot(density(prop_nonScottish))
            # origin.deciles <- quantile(prop_nonScottish, 0:10/10)
            # 
            # p2 <- cut(prop_nonScottish, origin.deciles, include.lowest=T, labels=F)
            # head(p2)
            # 
            
            ################################################################################
            # Do the same with 2001 Country of Origin Data
            
            # Pathos_Data
            #  : intermed [A]
            
            y1 <- subset(
                Census_2011_Lookup__OA_TO_HIGHER_AREAS,
                select=c(
                    "OutputArea2011Code",
                    "OutputArea2001Code",
                    "IntermediateZone2001Code"
                )
            )
            
            x.2001 <- subset(Data_Long, year==2001)
            
            x2 <- merge(
                x=x.2001,
                y=y1,
                
                by.x="intermed",
                by.y="IntermediateZone2001Code",
                all=F
            )
            
            # Census_2011_Lookup_OA_TO_HIGHER_AREAS
            #  : OutputArea2011Code
            #  : OutputArea2001Code [B]
            #  : IntermediateZone2001Code [A]
            
            x3 <- merge(
                x=x2,
                y=Census_2001_OA_Lookup,
                
                by="OutputArea2001Code",
                all=F
            )
            
            
            # Census_2001_OA_Lookup
            #   : OutputArea2001Code [B]
            #   : NRSoldOutputArea2001Code [C]
            
            x4 <- merge(
                x=x3, 
                y=Census_2001__KS005__Country_Of_Origin,
                
                by.x="NRSoldOutputArea2001Code",
                by.y="Zone.Code",
                all=F
            )
            
            merged.2001 <- x4
            rm(x1, x2, x3, x4, x.2001, y.2011, y1, y.ss)
            
            
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


