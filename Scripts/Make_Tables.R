# Make Tables

tmp <- xtable(ddply(Data_Long, year~variable, summarise, mean=mean(value), sd=sd(value)))
print.xtable(tmp, file="Tables/Pathos_Summary.html", type="html")
rm(tmp)

