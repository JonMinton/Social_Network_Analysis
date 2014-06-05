# Make Tables

tmp <- xtable(ddply(Data_Long, year~variable, summarise, mean=mean(value), sd=sd(value)))
print.xtable(tmp, file="Tables/Pathos_Summary.html", type="html")
rm(tmp)

print.xtable(
    xtable(head(Pathos_Data), label="Sample of Pathos_Data", digits=2),
    file="Tables/Pathos_Data_Sample.html", type="html"
    )

