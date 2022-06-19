#Hello world!

require(magrittr)
require(haven)
require(data.table)

# https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#vintage2020
# https://www.cdc.gov/nchs/data/nvss/bridged_race/Documentation-Bridged-PostcenV2020.pdf
# data dictionary starting on page 18 of th pdf
# RACESEX Bridged-race-sex Numeric
# 1=White male
# 2=White female
# 3=Black male
# 4=Black female
# 5=American Indian or Alaska Native male
# 6=American Indian or Alaska Native female
# 7=Asian or Pacific Islander male
# 8=Asian or Pacific Islander female

# HISP Hispanic origin Numeric
# 1=not Hispanic or Latino
# 2=Hispanic or Latino

temporaryFile <- tempfile()
"https://ftp.cdc.gov/pub/Health_Statistics/NCHS/nvss/bridged_race/pcen_v2020_y1020_sas7bdat.zip" %>%
  # "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2020_y1020_txt.zip" %>% 
  download.file(temporaryFile)

fileNameList <- as.character(unzip(temporaryFile, list = T)$Name)
fileNameList %>% print()

pcen2020 <- unzip(temporaryFile, fileNameList) %>% 
  read_sas %>% 
  as.data.table %>% 
  .[ST_FIPS == 41]

# cbind(male = seq(1, 8, 2), female = seq(2, 8, 2))# %>% as.data.table
# pcen2020 <- pcen2020[ST_FIPS == 41]
pcen2020[RACESEX %in% seq(1, 8, 2), sex := "Male"]
pcen2020[RACESEX %in% seq(2, 8, 2), sex := "Female"]

race4dt <- fread("race4, RaceName 
                 1, White 
                 2, Black 
                 3, American Indian or Alaska Native 
                 4, Asian or Pacific Islander")

pcen2020 <- pcen2020[race4dt, on = "race4"]
# dont' know why but 000 doesn't work so I had to separate 000 to 00,0
pcen2020[,PSTCO := ifelse(CO_FIPS < 10,
                          paste0(ST_FIPS, "00", CO_FIPS),
                          paste0(ST_FIPS, "0", CO_FIPS)) %>% as.numeric]

pcen2020[,.N, PSTCO]
