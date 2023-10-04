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

file_contents <- set_names(file_contents, file_paths)

# 2 Purrr Map

a<- 
  file_paths %>% 
  map(function (file_paths){
    read_excel(file_paths, sheet = 1 , skip = 4)
    
  })

# test load data
files <- list.files(path = "rawdata/FuelNumofVehicleRegistered_Monthly/", full.names = TRUE, pattern = ".xls")
# Write a reading function
read_excel_file <- function(file) {
  if(is.na(file)) stop("No file path") # TEST if path exists
  
  df <- readxl::read_excel(file, skip = 4)
  # add data cleaning / validation
  df
}

# test first excel file
read_excel_file(file = files[1])

# loop to read all excel files data
df_list <- purrr::map(.x = files, .f = read_excel_file)

file_paths %>% 
  map(function(file){ 
    map(excel_sheets(file), 
        function(sheet) {read_xlsx(file, sheet)})
    
    })
