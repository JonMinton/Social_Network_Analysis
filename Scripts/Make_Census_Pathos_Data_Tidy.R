if (file.exists("Data/RObj/Tidied_Data.RData")){
    load("Data/RObj/Tidied_Data.RData")
} else {
    census_tidy <- Make_Tidy_Census()
    pathos_tidy <- Make_Tidy_Pathos()
    save(
        census_tidy,
        pathos_tidy,
        file="Data/RObj/Tidied_Data.RData"
    )
    
}


