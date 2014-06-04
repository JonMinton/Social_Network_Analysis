


types <- c(
    "I", 
    "II", 
    "III", 
    "IV",
    "All",
    "Core"
)


Data_Long.nomiss <- Data_Long[NULL,]
# What if 0s are treated as missing

intermeds <- unique(Data_Long$intermed)

for (i in types){
    
    tmp.df <- subset(Data_Long, year==2001 & variable==i)
    tmp1 <- as.character(tmp.df$intermed[which(tmp.df$value==0)])
    tmp.df <- subset(Data_Long, year==2011 & variable==i)
    tmp2 <- as.character(tmp.df$intermed[which(tmp.df$value==0)])
    tmp3 <- union(tmp1, tmp2)
    
    tmp4 <- setdiff(intermeds, tmp3)
    tmp.df <- subset(Data_Long, variable==i)
    
    tmp.df <- tmp.df[tmp.df$intermed %in% tmp4,]
    Data_Long.nomiss <- rbind(df.nomiss, tmp.df)
    
}

rm(tmp.df,
   tmp1,
   tmp2,
   tmp3,
   tmp4
   )