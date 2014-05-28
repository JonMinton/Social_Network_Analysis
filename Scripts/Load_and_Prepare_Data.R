# Check for whether file has already been downloaded. If it has then don't download it again


# NOTE: The code below is very temperamental: it only seems to work once per session, and does not work
# on some machines due to memory limits being reached. However I cannot improve on it for now. 


if (!file.exists("Data/Raw/Full_Dataset.csv")){
    url <- "https://www.dropbox.com/s/3tqfbty79i2hpdl/full.dataset.csv"
    print("Cannot find dataset. Downloading.")
    dir.create("Data/Raw/", recursive=T, showWarnings=F)
    tmp <- httr::GET(url,
                     verbose()
    )
    writeBin(
        content(tmp, "raw"),
        "Data/Raw/Full_Dataset.csv"
    )
} else {
    print("Found Dataset so not downloading again.")
}


# Read the file into an R object format


Data <- read.csv("Data/Raw/Full_Dataset.csv")