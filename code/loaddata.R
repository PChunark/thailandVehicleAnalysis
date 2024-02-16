library("stringr")
library("readxl")
library("tidyverse")

files <- list.files(path = "rawdata/FuelNumofVehicleRegistered_Monthly/", pattern = ".xls")
files <- stringr::str_c("rawdata/FuelNumofVehicleRegistered_Monthly/", files)

a <- purrr::map_df(.x = files, .f = read_excel)

# Load multiple files 
# https://www.youtube.com/watch?v=An1bUIg-nVM

library(fs)
file_paths <- fs::dir_ls("rawdata/FuelNumofVehicleRegistered_Monthly")
file_paths[[1]]


# 2 Purrr Map

a <- 
map(file_paths,
    function(file){
      map(excel_sheets(file),
          function(sheet){
            read_excel(file,
                       sheet, 
                       # range = "A1:N52",
                       skip = 4) 
          })
    })

seq_along(excel_sheets(file_paths[[1]]))
set_names(file_paths, excel_sheets(file_paths[[1]]))

b <- 
  map(file_paths,
      function(file){
        map(excel_sheets(file),
            function(sheet){
              map(set_names(sheet),
                  function(sheet1){
                     read_excel(file,
                                sheet1, 
                              # range = "A1:N52",
                                skip = 4) %>% 
                      pivot_longer(-"ประเภทรถ",
                                   names_to ="category", 
                                   values_to = "unit") %>% 
                      filter(category == "ไฟฟ้า")
                    
                  })
            })
      }, .id = files)

sss <- data.frame(excel_sheets(file_paths[1]))
c<-
  file_paths %>% 
  map(function (path){
    read_excel(path, 
               skip = 4) %>% 
      set_names(., 
                nm = map(.x = .,
                         ~excel_sheets(file_paths[1])))
      
  })
  
# https://www.r4epi.com/using-the-purrr-package
d<- map(.x = excel_sheets(as.character(file_paths[1])),
     .f = function(x){
       new_nm <- tolower(x)
       assign(new_nm, read_excel(as.character(file_paths[1]), sheet = x, skip = 4), envir = .GlobalEnv)
     })

e <- map(
  .x = excel_sheets(as.character(file_paths[1])),
  .f = ~ read_excel(as.character(file_paths[1]), sheet = .x, skip = 4) %>% select("ประเภทรถ")
) 

bb<-b %>% flatten
