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

pcen2020 <- unz(temporaryFile, fileNameList) %>% 
  read_sas %>% 
  as.data.table %>% 
  .[ST_FIPS == 41]

unlink(temporaryFile)


# cbind(male = seq(1, 8, 2), female = seq(2, 8, 2))# %>% as.data.table
# pcen2020 <- pcen2020[ST_FIPS == 41]
pcen2020[RACESEX %in% seq(1, 8, 2), sex := "Male"]
pcen2020[RACESEX %in% seq(2, 8, 2), sex := "Female"]

race4dt <- fread("race4, raceName 
                 1, White 
                 2, Black or African American 
                 3, American Indian or Alaska Native 
                 4, Asian or Pacific Islander")

pcen2020 <- pcen2020[race4dt, on = "race4"]
# dont' know why but 000 doesn't work so I had to separate 000 to 00,0
pcen2020[,PSTCO := ifelse(CO_FIPS < 10,
                          paste0(ST_FIPS, "00", CO_FIPS),
                          paste0(ST_FIPS, "0", CO_FIPS)) %>% as.numeric]

pcen2020[,.N, PSTCO]

countyName.dt <- fread("countyName	CO_FIPS
                        Baker	1
                        Benton	3
                        Clackamas	5
                        Clatsop	7
                        Columbia	9
                        Coos	11
                        Crook	13
                        Curry	15
                        Deschutes	17
                        Douglas	19
                        Gilliam	21
                        Grant	23
                        Harney	25
                        Hood River	27
                        Jackson	29
                        Jefferson	31
                        Josephine	33
                        Klamath	35
                        Lake	37
                        Lane	39
                        Lincoln	41
                        Linn	43
                        Malheur	45
                        Marion	47
                        Morrow	49
                        Multnomah	51
                        Polk	53
                        Sherman	55
                        Tillamook	57
                        Umatilla	59
                        Union	61
                        Wallowa	63
                        Wasco	65
                        Washington	67
                        Wheeler	69
                        Yamhill	71")

pcen2020 <- pcen2020[countyName.dt, on = "CO_FIPS"]
pcen2020[hisp == 2, raceName := "Hispanic or Latino"]

# pcen2020[,.SD, .SDcols = "POP2020"]
# pcen2020[,.(pop2020 = sum(POP2020)), by = c("PSTCO", "raceName")] %>% summary#[,.N, raceName]
# pcen2020[,.N, raceName]

year.dt <- cbind(grep("20", names(pcen2020), value = T), seq(2009, 2020, 1)) %>%
  as.data.table

setnames(year.dt, names(year.dt), c("estimateYear", "Year"))

year.dt <- year.dt[Year > 2010]


pcen2020.long <- melt(pcen2020,
                      id.vars = c("age", "sex", "raceName", "PSTCO", "countyName"),
                      measure.vars = year.dt[,estimateYear],
                      variable.name = "estimateYear",
                      value.name = "population")
pcen2020.long <- pcen2020.long[year.dt, on = "estimateYear"]


# add age groups
ageGroup <- c("1-4 years", "5-9 years", "10-14 years", "15-19 years",
              "20-24 years", "25-34 years", "35-44 years", "45-54 years",
              "55-64 years", "65-74 years", "75-84 years", "85+ years")

cutAge <- c(0,
            seq(4, 24, 5),
            seq(24, 84, 10),
            Inf)

pcen2020[, ageGroup := age |> cut(breaks = cutAge |> unique(), ageGroup)]