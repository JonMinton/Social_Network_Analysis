# Figures

# Visualisation

png("Figures/Density_Lattice_Full.png", width=1200, height=800)
d <- ggplot(
    Data_Long,
    aes(
        x=value
    )
)
d2 <- d + geom_density(fill="black", colour="black") + facet_grid(year ~variable)
d3 <- d2 + xlim(-0.1, 0.1) + ylim(0, 50)
print(d3)
dev.off()

source("Scripts/Treat_Zeros_As_Missing.R")


png("Figures/Density_Lattice_No_Missings.png", width=1200, height=800)
d <- ggplot(
    Data_Long.nomiss,
    aes(
        x=value
    )
)
d2 <- d + geom_density(fill="black", colour="black") + facet_grid(year ~ variable)
d3 <- d2 + xlim(-0.1, 0.1) + ylim(0, 50)
print(d3)
dev.off()
