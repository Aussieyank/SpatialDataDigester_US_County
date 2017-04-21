# devtools::install_github("MangoTheCat/radarchart")
library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)
library(data.table)
library(formattable)
library(googleVis)
# library(crosstalk)
library(maps)
library(radarchart)
library(geosphere)
library(RColorBrewer)

options(digits=4)

fips_fix_dig <- function(vec,wid){
  # format county identifier FIPS to a given number of digits (adding "0" in front when needed)
  formatC(vec, width = wid, format = "d", flag = "0")
}

readcsv_fips_value <- function(filename, fipscol, valcol, valcolname,...){
  dat <- fread(filename, stringsAsFactors = FALSE,...)
  dat = as.data.frame(dat)
  dat <- dat[,c(fipscol,valcol)]
  dat[,1] <- fips_fix_dig(dat[,1],5)
  names(dat)[-1] = valcolname
  names(dat) <- tolower(names(dat))
  names(dat)[1] = "GEOID"
  return(dat)
}

votedat = readcsv_fips_value("./data/2016_US_County_Level_Presidential_Results.csv",
                             11,c(5,6),c("per_dem","per_gop"))
votedat$perdiff = votedat$per_dem - votedat$per_gop

colnames3_40_45 = c("Civilian_labor_force_2015", "Employed_2015", "Unemployed_2015",
                    "Unemployment_rate_2015", "Median_Household_Income_2015", "Med_HH_Income_Percent_of_State_Total_2015","county_state")

incdat = readcsv_fips_value("./data/Unemployment_Med_HH_Inc.csv",
                            1,c(40:45,3),colnames3_40_45,skip=7)

incdat$median_household_income_2015 = as.numeric(gsub("[$,]", "", incdat$median_household_income_2015))


# ====================
# living wage data -- 3 main cases:  2 adults 2 kids,  1 adult 1 kid, 1 adult

# had to change to UTF 8 encoding

# [1] "Hourly.Wages"                      
# [2] "X1.Adult"                          
# [3] "X1.Adult.1.Child"                  
# [4] "X1.Adult.2.Children"               
# [5] "X1.Adult.3.Children"               
# [6] "X2.Adults..One.Working."           
# [7] "X2.Adults..One.Working..1.Child"   
# [8] "X2.Adults..One.Working..2.Children"
# [9] "X2.Adults..One.Working..3.Children"
# [10] "X2.Adults"                         
# [11] "X2.Adults.1.Child"                 
# [12] "X2.Adults.2.Children"              
# [13] "X2.Adults.3.Children"              
# [14] "FIPS.Code"                         
# [15] "Location"                          
# [16] "year"    


# ---------
# readcsv_fips_value <- function(filename, fipscol, valcol, valcolname,...)
livingwagedat = readcsv_fips_value("./data/2015_Living_Wage.csv", 14, c(12,3,2),c("twoatwoc","oneaonec","onea"))

livingwagedat$twoatwoc = as.numeric(gsub("[$,]", "", livingwagedat$twoatwoc))
livingwagedat$oneaonec = as.numeric(gsub("[$,]", "", livingwagedat$oneaonec))
livingwagedat$onea     = as.numeric(gsub("[$,]", "", livingwagedat$onea))

livingwagedat[,2:4] = livingwagedat[,2:4]*40*52 # convert hourly wage to annual salary
# ====================


# crimedat = read.csv("./data/crime_data_w_population_and_crime_rate.csv")
# > names(crimedat)
# [1] "county_name"           "crime_rate_per_100000" "index"                 "EDITION"               "PART"                  "IDNO"                 
# [7] "CPOPARST"              "CPOPCRIM"              "AG_ARRST"              "AG_OFF"                "COVIND"                "INDEX"                
# [13] "MODINDX"               "MURDER"                "RAPE"                  "ROBBERY"               "AGASSLT"               "BURGLRY"              
# [19] "LARCENY"               "MVTHEFT"               "ARSON"                 "population"            "FIPS_ST"               "FIPS_CTY"   
crimedat = readcsv_fips_value("./data/crime_data_w_population_and_crime_rate.csv", 23, c(24,2,22),c("fips_last3","crime_rate_per_100k","population"))

crimedat$GEOID = paste0(fips_fix_dig(as.integer(crimedat$GEOID),2), fips_fix_dig(crimedat$fips_last3,3))

crimedat$fips_last3 = NULL


# > area = read.csv('./data/US_Counties.csv')
# > names(area)
# [1] "X_feature_id"        "X_feature_id_string" "the_geom"            "geo_id"              "state"               "county"              "name"                "lsad"               
# [9] "censusarea"          "shape_leng"          "shape_area"   


areadat = readcsv_fips_value("./data/US_Counties.csv", 4, 9,"censusarea")
areadat$GEOID = gsub("0500000US", "", areadat$GEOID)




# ttt1 = fips_fix_dig(crimedat$GEOID,2)
# ttt2 = fips_fix_dig(crimedat$fips_last3,3)

# ===================================================================
# filenames <- list.files("./data", pattern="*.csv", full.names=TRUE)
# ldf <- lapply(filenames, read.csv)
# res <- lapply(ldf, summary)
# names(res) <- substr(filenames, 8, 30)
# ===================================================================

# url = "https://data.cdc.gov/api/views/cjae-szjv/rows.csv?accessType=DOWNLOAD"
# dat <- read.csv(url, stringsAsFactors = FALSE)

# 1 -- read csv
dat <- fread("./data/Air_Quality_Measures_on_the_National_Environmental_Health_Tracking_Network.csv", stringsAsFactors = FALSE)
names(dat) <- tolower(names(dat))

# dat$countyname <- tolower(dat$countyname)

# -- debug --
# names(dat)
# count(dat,countyname)

# measure_per_year <- dat %>%
#   group_by(measureid,reportyear) %>%
#   summarise(n())

id_name <- dat %>%
  select(measureid,measurename,unit, unitname) %>%
  unique()


# Wide data set, subset only what we need.

# county_dat <- subset(dat, measureid == "296",
#                      select = c("reportyear","countyfips","statename", "countyname", "value", "unitname")) %>%
#   subset(reportyear==2011, select = c("countyfips", "value"))

airdat <- dat %>%
  filter(measureid == "296" & reportyear == 2011) %>%
  select(countyfips, value)
# select(measureid, measurename,countyfips, value,unitname)
colnames(airdat) <- c("GEOID", "airqlty")

# Have to add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
# airdat$GEOID <- formatC(airdat$GEOID, width = 5, format = "d", flag = "0")
airdat$GEOID <- fips_fix_dig(airdat$GEOID,5)

#  In 2015, Shannon County, SD (FIPS code 46113) is now Oglala Lakota County (FIPS code 46102)
airdat$GEOID        <- gsub("46113", "46102", airdat$GEOID)
votedat$GEOID       <- gsub("46113", "46102", votedat$GEOID)
livingwagedat$GEOID <- gsub("46113", "46102", livingwagedat$GEOID)
crimedat$GEOID      <- gsub("46113", "46102", crimedat$GEOID)

dona_ana <- data.frame(GEOID="35013",crime_rate_per_100k=210, population=214000)
crimedat = rbind(crimedat, dona_ana)   # filling data for FIPS = 35013,  Dona Ana County, NM

areadat$GEOID      <- gsub("46113", "46102", areadat$GEOID)


# ==== merge on GEOID =======
county_dat <- full_join(airdat, votedat, by=c("GEOID"))
county_dat <- full_join(county_dat, incdat, by=c("GEOID"))
county_dat <- full_join(county_dat, livingwagedat, by=c("GEOID"))

county_dat$r_inc_cos = county_dat$median_household_income_2015 / county_dat$twoatwoc

county_dat <- full_join(county_dat, crimedat, by=c("GEOID"))

county_dat <- full_join(county_dat, areadat, by=c("GEOID"))

county_dat$pop_den_log = log(county_dat$population / county_dat$censusarea)
# ===========================



# derivative data







# Rename columns to make for a clean df merge later.
# colnames(county_dat) <- c("GEOID", "airqlty", "gopvote")


# here generate a normalized version of county_dat, where each numeric column is shifted then normalized to a range of [0,1]
# this is done by  normed =  ( original_col - min_of_this_col ) / original_range_of_this_col


scalar = 10
# ====================== this can be made into a function ==================
nums <- sapply(county_dat, is.numeric)
to_be_normalized = county_dat[,nums]
normalized = as.data.frame(lapply(to_be_normalized, normalize, na.rm=TRUE))

county_dat_norm = county_dat
county_dat_norm[,nums] = normalized * scalar     # change to [0,scalar]
# ====================== next step is to set the "good" direction for some measurement ==================

# for example, income and air quality data has both been mapped to [0,1] range, 
# yet 1 means good for income, but bad for air quality.

# since these values will be used to generated a average "score" for each location, we want to have consistant "good" direction

# question still remains for subjective data like political vote results

county_dat_norm["airqlty"]                = scalar - county_dat_norm["airqlty"]
county_dat_norm["unemployment_rate_2015"] = scalar - county_dat_norm["unemployment_rate_2015"]
county_dat_norm["twoatwoc"]               = scalar - county_dat_norm["twoatwoc"]
county_dat_norm["oneaonec"]               = scalar - county_dat_norm["oneaonec"]
county_dat_norm["onea"]                   = scalar - county_dat_norm["onea"]

county_dat_norm$crime_rate_per_100k       = scalar - county_dat_norm$crime_rate_per_100k

# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html


# us.map <- readOGR(dsn = "./shape", layer = "cb_2013_us_county_20m", stringsAsFactors = FALSE)

# us.map <- readOGR(dsn = "./shape", layer = "cb_2016_us_county_500k", stringsAsFactors = FALSE)

us.map <- readOGR(dsn = "./shape", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)


# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

# ===========================================
# lat lon for each county
centroid = rgeos::gCentroid(us.map, byid=TRUE)

GEOID = us.map$GEOID
lat   = centroid$y
lon   = centroid$x

GEOID_lat_lon = data.frame(GEOID, lat, lon)

county_dat = dplyr::left_join(county_dat, GEOID_lat_lon, by=c("GEOID"))

# us.map <- merge(us.map, centroid)

# ===========================================

# Merge spatial df with downloade ddata.
leafmap <- merge(us.map, county_dat, by=c("GEOID"))

leafmap_norm <- merge(us.map, county_dat_norm, by=c("GEOID"))










dummy = as.data.frame(leafmap)

dummy_norm = as.data.frame(leafmap_norm)
# dummy <- dummy %>% arrange(desc(median_household_income_2015))

neededcols = c(10, 13, 18, 21, 24, 25, 28)
# neededcols = c("airqlty", "perdiff", "median_household_income_2015", "twoatwoc", "r_inc_cos","crime_rate_per_100k")



colnames = names(dummy)[neededcols]
mydf = dummy_norm %>%
  select(neededcols)




dummy2 = dummy %>%
  select(neededcols)

# find min max for all needed cols
MINs = apply(dummy2,2,min)  
MAXs = apply(dummy2,2,max)

MINs <- formattable(MINs, digits = 2, format = "f")
MAXs <- formattable(MAXs, digits = 2, format = "f")

# MINs <- round(MINs,2)
# MAXs <- round(MAXs,2)

mydf_ct = mydf


lon_lat_county_mat=cbind(dummy$lon, dummy$lat)


distm (lon_lat_county_mat, rbind(c(-80, 40)), fun = distHaversine)
