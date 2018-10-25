# DATA_MANIPUALATE_V5-1.R

# REQUIRED: 1.ladybirds.csv FROM PROJECTS DRIVE

# V5-1 MINOR CHANGES TO V5 TO MAINTAIN COMPATABILITY WITH PACKAGE UPDATES.

# V5 REVISES V4 BY:
# 1. CREATING HARL - A MATRIX OF 6685 MONADS BY 26 (N-1) YEARS OF (0,1) HARLEQUIN RESPONSES.

# V4 REVISES V3 BY CREATING A NEW HARL OBJECT SUCH THAT IT IS CONDENSED ONTO ONE COLUMN.

# V3 REVISES V2 BY:
# 1. LOADS IN DATA FROM ALL LADYBIRDS, NOT JUST THE NINE SPECIES OF PRIMARY INTEREST;
# 2. THIS ALSO REQUIRES THE START AND END DATES TO BE FORMATTED DIFFERENTLY;


# V2 REVISES V1 BY:
# 1. CORRECTLY RESOLVE TETRAD AND QUADRANT VALUES;
# 2. BEFORE CREATING THE FORMATTED_DATA OBJECT WE EXCLUDE THE HARLEQUIN RECORDS
# AND INSTEAD GENERATE THE YEAR OF FIRST ARRIVAL AT THE MONAD AND THE QUADRANT LEVEL,
# THIS BEING STORED AS YEAR AND A VECTOR;


rm(list=ls())
#library(anytime)
library(BRCmap)
library(fields)

# Load in the data:
lady <- read.csv("../Data/ladybirds.csv", header=TRUE)
colnames(lady) <- tolower(colnames(lady))
lady <- lady[, c("name", "startdate", "enddate", "gridref", "sq_10km", "quadrant", "tetrad", "precision", "gridref_type")]
summary(lady)
str(lady)
# Convert two date variables to date format:
lady$start <- as.Date(lady$startdate, format = "%d/%m/%Y")
lady$end <- as.Date(lady$enddate, format = "%d/%m/%Y")

###############################################################
# Ensure the dates are correct:
d1 <- as.numeric(substr(lady$startdate,1,2))
d2 <- as.numeric(substr(lady$start, 9,10))
m1 <- as.numeric(substr(lady$startdate,4,5))
m2 <- as.numeric(substr(lady$start, 6,7))
y1 <- as.numeric(substr(lady$startdate,7,10))
y2 <- as.numeric(substr(lady$start, 1,4))
df <- as.data.frame(cbind(d1,d2,m1,m2,y1,y2))
summary(df)
str(df)    
diff_d <- df$d1 - df$d2
table(diff_d, exclude=NULL)
diff_m <- df$m1 - df$m2
table(diff_m, exclude=NULL)
diff_y <- df$y1 - df$y2
table(diff_y, exclude=NULL)
rm(d1); rm(d2); rm(m1); rm(m2); rm(y1); rm(y2); rm(df)
rm(diff_d); rm(diff_m); rm(diff_y)

lady$startdate <- NULL
lady$enddate <- NULL
###############################################################

#  Now create the monad grid reference:
table(lady$precision)
lady$monad <- as.factor(reformat_gr(lady$gridref, prec_out = 1000, precision = NULL))

###############################################################
# Now verify for various precisions:
ltemp1 <- subset(lady, precision==1000)   # GRIDREF = MONAD
ltemp2 <- subset(lady, precision==100)    
ltemp3 <- subset(lady, precision==10)
ltemp4 <- subset(lady, precision==1)
head(ltemp1)
head(ltemp2)
head(ltemp3)
head(ltemp4)
rm(ltemp1); rm(ltemp2); rm(ltemp3); rm(ltemp4)

###############################################################

summary(lady)

# For plotting to assess the distribution of species over time,
# we need to calculate the Eastings and Northings:
lady$en <- gr_let2num(lady$monad, centre = TRUE, gr_prec = NULL, return_projection = FALSE) 
lady$Easting <- lady$en$EASTING
lady$Northing <- lady$en$NORTHING

# Add year of start date:
lady$year <- as.numeric(substr(lady$start,1,4))
table(lady$year)
# Records from 1600 onwards - what about the Harlequin?
table(lady$name)
harlequin <- subset(lady, name=="Harmonia axyridis")
table(harlequin$year)
# Harlequin invaded UK in 2003, so when to restrict data from?
# Nick suggests from 1990 - 13 years before and after as data only until 2016

# Examine records from 1990-2016 for all ladybird species:
lady <- subset(lady, year>=1990)
lady$monad <- factor(lady$monad)
# Still retains 180,000/210,000 ladybird records.

# And keep just those records where the start and end date are the same:
lady <- subset(lady, start == end)

# Drop the Northern Ireland records - 
# those that have monad beginning with a single letter:
lady$ch2 <- substr(lady$monad, 1,2)
lady$ch1 <- substr(lady$monad, 1,1)
table(lady$gridref_type)
x <- subset(lady, gridref_type=="CI" | gridref_type=="EMPTY")
table(x$ch1)
table(x$ch2)
# WV is the Channel Islands, so keep only the OSGB records:
lady <- subset(lady, gridref_type=="OSGB")
rm(x)


lady$monad <- factor(lady$monad)
head(lady[, c("monad", "tetrad", "quadrant", "sq_10km")])
# Tetrad and quadrant have not been correctly resolved:
lady$tetrad <- NULL
lady$quadrant <- NULL
lady$tetrad <- as.factor(reformat_gr(lady$gridref, prec_out = 2000, precision = NULL))
lady$quadrant <- as.factor(reformat_gr(lady$gridref, prec_out = 5000, precision = NULL))
head(lady[, c("monad", "tetrad", "quadrant", "sq_10km")])

# Restrict records to those with precision which can be reoslved to moand or finer:
lady <- subset(lady, precision<=1000)

# Sort data frame:
lady <- lady[order(lady$monad, lady$year),]

# Now create separate species data frames:
harlequin <- subset(lady, name=="Harmonia axyridis")
two_spot <- subset(lady, name=="Adalia bipunctata")
ten_spot <- subset(lady, name=="Adalia decempunctata")
cream_spot <- subset(lady, name=="Calvia quattuordecimguttata")
seven_spot <- subset(lady, name=="Coccinella septempunctata")
pine <- subset(lady, name=="Exochomus quadripustulatus")
orange <- subset(lady, name=="Halyzia sedecimguttata")
fourteen_spot <- subset(lady, name=="Propylea quattuordecimpunctata")
twentytwo_spot <- subset(lady, name=="Psyllobora vigintiduopunctata")

# Quick examination of number of monads, per species:
str(lady$monad)    # 29,997 monads overall
harlequin$monad <- factor(harlequin$monad)
str(harlequin$monad)  # 15,975 monads
two_spot$monad <- factor(two_spot$monad)
str(two_spot$monad)  # 5,618 monads
ten_spot$monad <- factor(ten_spot$monad)
str(ten_spot$monad)  # 3,113 monads
cream_spot$monad <- factor(cream_spot$monad)
str(cream_spot$monad)  # 2,352 monads
seven_spot$monad <- factor(seven_spot$monad)
str(seven_spot$monad)  # 13,261 monads
pine$monad <- factor(pine$monad)
str(pine$monad)  # 2,073 monads
orange$monad <- factor(orange$monad)
str(orange$monad)  # 2,721 monads
fourteen_spot$monad <- factor(fourteen_spot$monad)
str(fourteen_spot$monad)  # 5,445 monads
twentytwo_spot$monad <- factor(twentytwo_spot$monad)
str(twentytwo_spot$monad)  # 2,714 monads

# Create unique records of monad & year, per species -
# to do this across all species requires list length across
# ALL species in the BRC

# Let's start with Harlequin ladybird:
harl2 <- unique(harlequin[c("monad", "year")])
str(harl2)
# 25,692 unique monad/year values from 15,975 unique monads
# This implies that most monads have just a single year of records.

# Then the seven-spotted ladybird:
seven2 <- unique(seven_spot[c("monad", "year")])
str(seven2)
# 19,399 unique monad/years from 13,261 unique monads.

# And also with the least common ladybird, pine:
pine2 <- unique(pine[c("monad", "year")])
str(pine2)
# 2905 unique monad/years from 2073 unique monads.

# Indeed can get the number of rows by using:
dim(unique(harlequin[c("monad", "year")]))[1]       # 15,975/25,692
dim(unique(seven_spot[c("monad", "year")]))[1]      # 13,261/19,399
dim(unique(pine[c("monad", "year")]))[1]            # 2,073/2,905
dim(unique(two_spot[c("monad", "year")]))[1]        # 5,618/7,965
dim(unique(ten_spot[c("monad", "year")]))[1]        # 3,113/3,896
dim(unique(cream_spot[c("monad", "year")]))[1]      # 2,352/2,830
dim(unique(orange[c("monad", "year")]))[1]          # 2,721/3,520
dim(unique(fourteen_spot[c("monad", "year")]))[1]   # 5,445/7,153
dim(unique(twentytwo_spot[c("monad", "year")]))[1]  # 2,714/3,426


harl_2003 <- subset(harl2, year==2003)
harl_2004 <- subset(harl2, year==2004)
# harl_2005 <- subset(harl2, year==2005)
# harl_2006 <- subset(harl2, year==2006)
# harl_2007 <- subset(harl2, year==2007)
# harl_2008 <- subset(harl2, year==2008)
# harl_2009 <- subset(harl2, year==2009)
# harl_2010 <- subset(harl2, year==2010)
# harl_2011 <- subset(harl2, year==2011)
# harl_2012 <- subset(harl2, year==2012)
# harl_2013 <- subset(harl2, year==2013)
# harl_2014 <- subset(harl2, year==2014)
# harl_2015 <- subset(harl2, year==2015)
# harl_2016 <- subset(harl2, year==2016)
# 

# Set up a list to capture the annual monads
yr_subs = vector("list",length(2003:2016))
names(yr_subs) = 2003:2016

#tiff("Results/Harlequin ladybird distribution over time_v2.tiff", width=1000, height=1000)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,0), 4, 4, byrow = TRUE))
data(UK)

for(j in 2003:2016){
  num <- paste0("x_", j)
  num <- subset(harl2, year==j)
  yr_subs[[as.character(j)]] = num
  #num2 <- get(num)
  year <- print(j)
  print(length(num$monad))
  plot_GIS(UK, new.window = FALSE, main = j, show.axis = FALSE, show.grid = FALSE, 
           xlab = "", ylab = "")
  plotUK_gr(num$monad, col="red") # , border = NA)
}
#dev.off()

summary(yr_subs)
# First year has only two records:
yr_subs[1]

# Helen has suggested examining the single year monads to ascertain if 
# grid references are at the boundary......but this is a red herring.....

# WE NEED THE FOLLOWING DATA:
# 1. ALL BRC VISITS 1990 TO 2016; - THIS STILL NEEDS CLARIFYING
# 2. KEEP JUST RECORDS WITH STARTDATE = ENDDATE; - DONE
# 3. JUST GB RECORDS - NOT NORTHERN IRELAND; - DONE
# 4. PRECISION AT LEAST OF 1KM; - DONE
# 5. HARLEQUIN: 2003 TO 2016 - BUT GENERATE A VARIABLE FOR THE YEAR OF FIRST ARRIVAL INTO:
# 5A. MONAD
# 5B. QUADRANT
# THIS MAY HAVE TO BE ULTIMATELY STORED AS C(0,0,0,00,0,1,1,1);
# 6. CALCULATE LIST LENGTH FOR ALL SPECIES - STRIPPING OUT HARLEQUIN BEFOREHAND -
# MAY REQUIRE THE FORMATOCCDATA.R SCRIPT (SPARTA) TO DO THIS -
# THIS PRODUCESS A FORMATTED_DATA OBJECT WITH TWO DATA FRAMES:
# 6A. SPP_VIS: VISIT/SPECIES DATA FRAME WITH TRUE/FALSE ENTRIES, PER VISIT (ROW)
# 6B. OCCDETDATA: SITE, LIST LENGTH AND YEAR, PER VISIT(ROW);
# 7. LIMIT SITES/MONADS TO THOSE WITH 2+ YEARS OF DATA (ANY BRC VISIT),
# REGARDLESS OF WHETHER ANY LADYBIRD IS INCLUDED IN THE RECORD.

# COVARIATES: 
# 1. HARLEQUIN ARRIVED YET (YEAR OF ARRIVAL PROXY);
# 2. LIST LENGTH (NOT INC. HARLEQUIN, BUT INCLUDING ALL OTHER SPECIES).


# GENERATE THE YEAR OF FIRST ARRIVAL FOR THE HARLEQUIN AT THE 
# MONAD, TETRAD AND QUADRANT LEVELS:
harlequin$monad <- factor(harlequin$monad)
harlequin$tetrad <- factor(harlequin$tetrad)
harlequin$quadrant <- factor(harlequin$quadrant)
harlequin$sq_10km <- factor(harlequin$sq_10km)

harl_monad_year1 <- tapply(harlequin$year, harlequin$monad, min)
harl_monad_year1 <- data.frame(monad = names(harl_monad_year1),
                               harl_monad_year1 = as.numeric(harl_monad_year1))
#harl_monad_year1$monad <- row.names(harl_monad_year1)
#row.names(harl_monad_year1) <- NULL
#harl_monad_year1 <- harl_monad_year1[, c("monad", "harl_monad_year1")]
harl_monad_year1$monad <- factor(harl_monad_year1$monad)

harl_tetrad_year1 <- tapply(harlequin$year, harlequin$tetrad, min)
harl_tetrad_year1 <- as.data.frame(harl_tetrad_year1)
harl_tetrad_year1$tetrad <- row.names(harl_tetrad_year1)
row.names(harl_tetrad_year1) <- NULL
harl_tetrad_year1 <- harl_tetrad_year1[, c("tetrad", "harl_tetrad_year1")]
harl_tetrad_year1$tetrad <- factor(harl_tetrad_year1$tetrad)

harl_quadrant_year1 <- tapply(harlequin$year, harlequin$quadrant, min)
harl_quadrant_year1 <- as.data.frame(harl_quadrant_year1)
harl_quadrant_year1$quadrant <- row.names(harl_quadrant_year1)
row.names(harl_quadrant_year1) <- NULL
harl_quadrant_year1 <- harl_quadrant_year1[, c("quadrant", "harl_quadrant_year1")]
harl_quadrant_year1$quadrant <- factor(harl_quadrant_year1$quadrant)

harl_hectad_year1 <- tapply(harlequin$year, harlequin$sq_10km, min)
harl_hectad_year1 <- as.data.frame(harl_hectad_year1)
harl_hectad_year1$hectad <- row.names(harl_hectad_year1)
row.names(harl_hectad_year1) <- NULL
harl_hectad_year1 <- harl_hectad_year1[, c("hectad", "harl_hectad_year1")]
harl_hectad_year1$hectad <- factor(harl_hectad_year1$hectad)

summary(harl_monad_year1)
summary(harl_tetrad_year1)
summary(harl_quadrant_year1)
summary(harl_hectad_year1)

head(harl_monad_year1)
# Rename the data frame:
monads <- harl_monad_year1
rm(harl_monad_year1)
# So, the first monad, HU3455, would have a vector of the form:
# c(22 x 0, 5 x1)
names(monads)[names(monads) == 'harl_monad_year1'] <- 'year1'

for(i in 1990:2016){
  x <- paste0("yr_", i)
  monads[,x] <- ifelse(monads$year1<=i, 1, 0)
}
summary(monads)

# THEN EXTRACT ALL THE GB MONADS, OTHER THAN CHANNEL ISLANDS...
# ALL OTHER MONADS HAVE ZEROES FOR ALL YEARS.....


########################   START HERE ################################
########## WAIT UNTIL HELEN HAS RESOLVED THE THREE ISSUES RELATING TO
########## SPECIES NAMES (EMAIL SENT: 12.2.18).

##### 2. GET THE SPECIES NAMES LIST CORRECT - CHECK WITH HELEN
##### FOR TIME BEING ASSUME:
###### 1. ANY GENUS NAME ALONE CAN BE DROPPED 
###### 2. ANY FORMS CAN BE DROPPED
lady$name <- factor(lady$name)
table(lady$name)
lady <- subset(lady, name!="Adalia" & 
                     #name!="Adalia bipunctata forma quadrimaculata" &
                     #name!="Adalia bipunctata forma sexpustulata" & 
                     #name!="Adalia bipunctata forma typica" &
                     #name!="Adalia decempunctata forma bimaculata" &
                     #name!="Adalia decempunctata forma decempustulata" &
                     name!="Anatis" &
                     name!="Anisosticta" &
                     name!="Aphidecta" &
                     name!="Calvia" &
                     name!="Coccidula" &
                     name!="Exochomus" &
                     name!="Halyzia" &
                     name!="Harmonia" &
                     name!="Hippodamia" &
                     name!="Myrrha" &
                     name!="Myzia" &
                     name!="Nephus" &
                     name!="Propylea" &
                     name!="Psyllobora" &
                     name!="Rhyzobius" &
                     name!="Scymnus" &
                     name!="Subcoccinella" &
                     name!="Tytthaspis")
lady$name <- factor(lady$name)
table(lady$name)
length(lady$name)

# Two Adalia species have sub-species, so need to convert these to species level:
lady$name2 <- substr(lady$name, 1,17)
table(lady$name2)
#lady$name <- ifelse(lady$name2=="Adalia bipunctata", "Adalia bipunctata", lady$name)
lady$name3 <- ifelse(as.character(lady$name2)=="Adalia bipunctata", "Adalia bipunctata", as.character(lady$name))
lady$name3 <- ifelse(as.character(lady$name2)=="Adalia decempunct", "Adalia decempunct", as.character(lady$name3))
table(lady$name3)
lady$name <- NULL
lady$name2 <- NULL
names(lady)[names(lady) == 'name3'] <- 'name'
table(lady$name)

# Now have a look at list length using the sparta package:
# Strip the lady dataset of the Harlequin:
lady2 <- subset(lady, name!="Harmonia axyridis")
lady2$name <- factor(lady2$name)

str(lady2$monad)    # 29,997 monads - with Harlequin in
lady2$monad <- factor(lady2$monad)
str(lady2$monad)    # 21,363 monads excluding Harlequin

library(sparta)
formatted_data <- formatOccData(taxa = lady2$name,
                                site = lady2$monad,
                                survey = lady2$start)
table(formatted_data$occDetdata$L)
length(formatted_data$occDetdata$L)
46093 + 9724 + 3943 + 1883 + 930 + 505 + 255 + 127 + 68 + 47 + 20 + 7 + 2 + 4 + 1 
# There are 63,609 survey visits - of which 74% are single species visits.
1*46093 + 2*9724 + 3*3943 + 4*1883 + 5*930 + 6*505 + 7*255 + 8*127 + 9*68 + 10*47 + 11*20 + 12*7 + 13*2 + 14*4 + 15*1 + 21085
# lady2 df has 117,951 (species/survey vist) records, of which 21,085 are duplicates:

head(formatted_data$occDetdata)

# Calculate the number of unique years, per site - in this case, monad:
number_of_years <- tapply(formatted_data$occDetdata$TP, formatted_data$occDetdata$site, FUN = function(x) length(unique(x)))
length(number_of_years)
table(number_of_years)
# 70% of these records have just a single year of non-Harlequin ladybird records.

# Names of sites with 2+ years data
site_with_enough_years <- names(number_of_years)[number_of_years >= 2]
# 6685 monads have 2+ years with survey visits.

# subset the two resulting data frames (from formatted_data):
# 1. occDetdata:
df1 <- formatted_data$occDetdata[as.character(formatted_data$occDetdata$site) %in% site_with_enough_years,]
length(unique(df1$site))   # 6685 monads from 45,824 records
# 2. spp_vis:
df2 <- formatted_data$spp_vis[formatted_data$spp_vis$visit %in% unique(df1$visit), ]
length(df2$visit)   # 45,824 records



# NOW EXAMINE HOW THE HARLEQUIN MONADS AND THE NON-HARLEQUIN RECORDS MATCH UP:

# MERGE DF1 AND DF2 TOGETHER - WILL RETAIN ALL 45,824 RECORDS:
df <- merge(df1, df2, by="visit")
# Keep only the eight community ladybird species columns, first agreed -
# NOTE THAT ADALIA DECEMPUCTATA HAS BEEN SHORTENED BY THREE LETTERS TO FIT WITH
# THE RENAMING OF THE TWO ADALIA SPECIES:
df <- df[, c("visit", "site", "L", "TP", "Adalia bipunctata", "Adalia decempunct",
             "Calvia quattuordecimguttata", "Coccinella septempunctata", 
             "Exochomus quadripustulatus", "Halyzia sedecimguttata",
             "Propylea quattuordecimpunctata", "Psyllobora vigintiduopunctata")]

# THEN MERGE THIS WITH THE MONADS DATA FRAME - YEARS OF HARLEQUIN PRESENCE:
names(df)[names(df) == 'site'] <- 'monad'
df <- merge(df, monads, by="monad", all.x=TRUE)
summary(df)
str(df)
df$monad <- factor(df$monad)
# So, there are 13,926 survey visits with no Harlequin records -
# therefore we assume this has never arrived.
# We can check what years these records come from:
x <- subset(df, is.na(year1))
table(x$TP)
# All years have between 200-900 records with no Harlequin appearing....
rm(x)

for(i in 1990:2016){
  x <- paste0("yr_", i)
  print(i)
  print(table(df[,x], exclude=NULL))
  df[,x] <- ifelse(is.na(df[,x]), 0, df[,x])
  print(table(df[,x], exclude=NULL))
}


# Now consider getting the data into a BUGS style object:
# Sort the df data frame by visit (monad/survey visit)
df <- df[with(df, order(visit)), ]

# THE NEXT SET OF CODE COMES FROM NICK'S MUNGE FILE:
# Get the identities of all the monads
monads <- unique(df$monad)

# when indexing by visit (in the observation model) convert Site identities into row numbers
s2v_lookup <- data.frame(monad=monads,rownum=1:length(monads)) 

# convert this to a vector of length = nrow(df)
s2v <- s2v_lookup$rownum[match(df$monad, s2v_lookup$monad)]
# these row numbers are used to indexing the site when looping through each visit

# Create the focal matrix:
focal <- df[,grepl(' ', names(df))]

# Colin has written a function to remove the dimnames of the
# year variables within df:
rm_dimnames = function(df){
  for(i in 1:ncol(df)){
    if(!is.null(dimnames(df[,i]))){
      dimnames(df[,i]) = NULL
    }
  }
  return(df)
}
df <- rm_dimnames(df)

### SET YEAR1 TO 9999 FOR THOSE MONADS WHERE HARLEQUIN HAS YET TO BE RECORDED:
#df$year1 <- ifelse(is.na(df$year1), 9999, df$year1)
### NOW CREATE A BINARY HARLEQUIN RECORD, INDICATING WHETHER HARLEQUIN HAS
### ARRIVED INT HE MONAD AT THE TIME OF THE SURVEY VISIT:
df$HARL <- ifelse(df$TP>= df$year1, 1, 0)
df$HARL <- ifelse(is.na(df$HARL), 0, df$HARL)
head(df[, c("monad", "visit", "TP", "year1", "HARL")])
df[10001:10050, c("monad", "visit", "TP", "year1", "HARL")]
tail(df[, c("monad", "visit", "TP", "year1", "HARL")], n=50)

HARL <- matrix(, nrow = length(unique(df$monad)), ncol = length(unique(df$TP))-1 )   # matrix

HARL2 <- unique(df[, c("monad", "year1")])   # data frame
for(i in 1990:2015){
  x <- paste0("yr_", i)
  HARL2[,x] <- ifelse(i < HARL2$year1, 0, 1)
  HARL2[,x] <- ifelse(is.na(HARL2[,x]), 0, HARL2[,x])
}
HARL <- as.matrix(HARL2[,3:28])
summary(HARL)



lady_data <- list(
  # metadata
  nyear=length(unique(df$TP)), 
  nsite=length(unique(df$monad)), 
  nvisit=nrow(df),
  
  # observation model data    
  Site=s2v, 
  Year= 1 + df$TP - min(df$TP),  # So that the earliest year is numbered as 1
  DATATYPE1 = (df$L==1)*1,
  DATATYPE2 = (df$L==2 | df$L==3)*1, 
  DATATYPE3 = (df$L>3)*1,
  Harl_year1 = 1 + df$year1 - min(df$TP),
  #HARL = df$HARL,
  HARL = HARL,
  
  # species data - for now put in all of it. Later we restrict it to one
  #focal=as.matrix(VisData[,grepl('Hym', names(VisData))]),
  focal=as.matrix(focal)*1
  
  # state model data
  # Just intercept? 
)

attr(lady_data,'s2v_lookup') <- s2v_lookup


###### THEN NEED TO CHECK THAT MONAD/VISIT SPECIES RECORDED MATCH
###### WITH THAT OF THE LADY DATAFRAME.

table(df$TP, lady_data$Year)
table(df$L, lady_data$DATATYPE1)
table(df$L, lady_data$DATATYPE2)
table(df$L, lady_data$DATATYPE3)
table(df$year1, lady_data$Harl_year1)
table(df$`Adalia bipunctata`, lady_data$focal[,1])
table(df$`Adalia decempunct`, lady_data$focal[,2])
table(df$`Calvia quattuordecimguttata`, lady_data$focal[,3])
table(df$`Coccinella septempunctata`, lady_data$focal[,4])
table(df$`Exochomus quadripustulatus`, lady_data$focal[,5])
table(df$`Halyzia sedecimguttata`, lady_data$focal[,6])
table(df$`Propylea quattuordecimpunctata`, lady_data$focal[,7])
table(df$`Psyllobora vigintiduopunctata`, lady_data$focal[,8])
# No real direct comparison for the single binary year:
x <- subset(df, year1<=year)
table(x$year1, x$HARL)  # A column of 1s, as expected.
# ONLY SITE REFERENCE JUST TO CHECK:
x1 <- df[40001:40090,]
x1$monad <- factor(x1$monad)
x2 <- lady_data$Site[40001:40090]
table(x1$monad, x2)
x3 <- subset(s2v_lookup, rownum>=5935 & rownum<=5935)
x3
rm(x1); rm(x2); rm(x3)
# CHANGE NUMBERS: 1:90; 1001:1090; 5001:5090; 10001:10090; 20001:20090; 40001:40090
# THEY ALL MATCH - OR AT LEAST THERE ARE NO OBVIOUS MIS-MATCHES.....


# NOW SAVE THE BUGS DATA OBJECT:
datecode <- format(Sys.Date(), '%y%m%d')
save(lady_data, file=paste0('Results/ladybird_',datecode,'.RData'))
