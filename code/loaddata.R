library("stringr")
library("purrr")
library("readxl")

files <- list.files(path = "rawdata/FuelNumofVehicleRegistered_Monthly/", pattern = ".xls")
files <- stringr::str_c("rawdata/FuelNumofVehicleRegistered_Monthly/", files)

a <- purrr::map_df(.x = files, .f = read_excel)

# Load multiple files 
# https://www.youtube.com/watch?v=An1bUIg-nVM

library(fs)
file_paths <- fs::dir_ls("rawdata/FuelNumofVehicleRegistered_Monthly")
file_paths[[1]]

# 1 for loop

file_contents <- list()

for (i in seq_along(file_paths)) {
  file_contents[[i]] <- read_excel(
    file_paths[[i]],sheet = 1, skip = 4
  )
}


read_excel("rawdata/FuelNumofVehicleRegistered_Monthly/fuel28Feb23.xls",
           sheet = 1, skip = 4)

read_excel("rawdata/FuelNumofVehicleRegistered_Monthly/fuel28Feb23.xls",
           sheet = 1, skip = 4)
excel_sheets(file_paths[[1]])
