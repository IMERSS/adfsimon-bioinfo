library(pdftools)

setwd("/Users/andrewsimon/Sync/Simon/Biodiversity_Galiano_Project/Species_Lists/Galiano_Occurrence_Records/K_Erickson")

files <- list.files(pattern = "pdf$")

erickson <- lapply(files, pdf_text)


lapply(erickson, length) 

erickson
