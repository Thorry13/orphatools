## code to prepare `DATASET` dataset goes here
source('data-raw/pack-2023_en.R')
source('data-raw/genes-202312_en.R')
source('data-raw/dict_en.R')

usethis::use_data(pack_data, genes_data, df_dict, internal=TRUE, overwrite = TRUE)
