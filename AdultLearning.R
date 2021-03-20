library(tidyverse)

#...set your working directory here...
setwd("~/DSAP/DATA - Swansea University contract/data08")

#defunct code
#enrol_08 <- read.csv('C:\\Users\\colli\\Documents\\DSAP\\csvFilesDSAP\\csvFilesDSAP\\Data Accelerator Programme\\DATA - Swansea University contract\\Ac Yr FINAL 2007-08\\ENROLEMENT.CSV',header = FALSE)

#define list of headers for enrolment file from the guidance notes

#...call this list something else perhaps...

#defining header lists for the files to be read in
#'enrolment', 'location', 'module', 'student', 'tutormodule' and from 2012 onwards, 'entry'
header_en <- list("provider_code", 
                      "student_id", 
                      "module_id", 
                      "enrolment_date", 
                      "enrolment_status",
                      "employer_role",
                      "employment_status",
                      "fee_code",
                      "fee_amount",
                      "instalments",
                      "withdrawal_date",
                      "withdrawal_reason",
                      "withdrawal_code",
                      "mode_study"
                      )

header_en1 <- list("provider_code", 
                  "student_id", 
                  "module_id", 
                  "enrolment_date", 
                  "enrolment_status",
                  "employer_role",
                  "employment_status",
                  "fee_code",
                  "fee_amount",
                  "instalments",
                  "withdrawal_date",
                  "withdrawal_reason",
                  "withdrawal_code",
                  "mode_study",
                  "tt_postcode",
                  "tt_accommodation"
)

header_lo <- list("provider_code",
                  "location_code",
                  "location_name",
                  "location_type",
                  "location_snam",
                  "add1",
                  "add2",
                  "add3",
                  "add4",
                  "postcode",
                  "telephone",
                  "local_authority"
                  )

header_tumo <- list("provider_code",
                "module_id",
                "tutor_id"
                )

header_stu <- list("provider_code",
                   "student_id",
                   "husid",
                   "title",
                   "surname",
                   "name1",
                   "name2",
                   "name3",
                   "known_as",
                   "surname_at_16",
                   "gender",
                   "dob",
                   "welsh_speaker",
                   "disability",
                   "ethnicity",
                   "domicile",
                   "natid1",
                   "natid2",
                   "nationality",
                   "highest_qual",
                   "disability_allowance",
                   "uln",
                   "hwebsite",
                   "hbrochure",
                   "hflyer",
                   "hadvert",
                   "hvenue",
                   "htutor",
                   "hfriend",
                   "do_not_contact",
                   "deceased",
                   "email",
                   "add1",
                   "add2",
                   "add3",
                   "add4",
                   "postcode"
                   )

header_stu1 <- list("provider_code",
                   "student_id",
                   "husid",
                   "title",
                   "surname",
                   "name1",
                   "name2",
                   "name3",
                   "known_as",
                   "surname_at_16",
                   "gender",
                   "dob",
                   "welsh_speaker",
                   "disability",
                   "ethnicity",
                   "domicile",
                   "natid1",
                   "natid2",
                   "nationality",
                   "highest_qual",
                   "disability_allowance",
                   "uln",
                   "hwebsite",
                   "hbrochure",
                   "hflyer",
                   "hadvert",
                   "hvenue",
                   "htutor",
                   "hfriend",
                   "do_not_contact",
                   "deceased",
                   "email",
                   "add1",
                   "add2",
                   "add3",
                   "add4",
                   "postcode",
                   "tel_day",
                   "tel_eve",
                   "mobile"
)

header_stu2 <- list("provider_code",
                    "student_id",
                    "husid",
                    "title",
                    "surname",
                    "name1",
                    "name2",
                    "name3",
                    "known_as",
                    "surname_at_16",
                    "gender",
                    "dob",
                    "welsh_speaker",
                    "disability",
                    "ethnicity",
                    "domicile",
                    "natid1",
                    "natid2",
                    "nationality",
                    "highest_qual",
                    "disability_allowance",
                    "uln",
                    "hwebsite",
                    "hbrochure",
                    "hflyer",
                    "hadvert",
                    "hvenue",
                    "htutor",
                    "hfriend",
                    "do_not_contact",
                    "deceased",
                    "email",
                    "add1",
                    "add2",
                    "add3",
                    "add4",
                    "postcode",
                    "tel_day",
                    "tel_eve",
                    "mobile",
                    "kin_surname",
                    "kin_name1",
                    "kin_name2",
                    "kin_name3",
                    "kin_add1",
                    "kin_add2",
                    "kin_add3",
                    "kin_add4",
                    "kin_postode",
                    "kin_tel_day",
                    "kin_tel_eve",
                    "kin_mob",
                    "kin_relationship",
                    "emergency_contact"
)

header_mo <- list("provider_code",
                  "module_id",
                  "module_title",
                  "contact_hours",
                  "max_students",
                  "min_students",
                  "fe_qualaim",
                  "module_mode_study",
                  "unit_length",
                  "length",
                  "location_code",
                  "module_start_date",
                  "module_end_date",
                  "module_start_time",
                  "module_end_time",
                  "cancelled_date",
                  "abandoned_date",
                  "taught_mon",
                  "taught_tue",
                  "taught_wed",
                  "taught_thu",
                  "taught_fri",
                  "taught_sat",
                  "taught_sun"
                  )

header_entry <- list("provider_code",
                     "student_id",
                     "module_id",
                     "highest_qual",
                     "domicile",
                     "previous_inst",
                     "year_left_inst",
                     "degree_where",
                     "degree_when",
                     "postcode")

#defunct code
#no need for this loop - see below
#for (i in 1:ncol(enrol_08)){
#  colnames(enrol_08)[i] = enrol_08_head[i]
#}

#defunct code - we do this later
#colnames here assigns the header list above to the first row
#colnames(enrol_08) <- enrol_08_head
#View(enrol_08)

#what are the directory files? - this lists all files within the path directory
#                                 so whatever the 'setwd' says is where it looks,
#                                 and then looks for CSV - case sensitive
filenames <- list.files(path = ".",pattern = ".CSV")
#all files
filenames
#first file in list
filenames[1]

#this loops the files and names them file1, file2 etc - note header = FALSE and na.strings
for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

#header = FALSE leaves space for us to assign the header titles from our header list
colnames(file1) <- header_en
colnames(file2) <- header_lo
colnames(file3) <- header_mo
colnames(file4) <- header_stu
colnames(file5) <- header_tumo
en08 <- file1 %>% mutate(year = 2008) %>% select(year, everything())
lo08 <- file2 %>% mutate(year = 2008) %>% select(year, everything())
mo08 <- file3 %>% mutate(year = 2008) %>% select(year, everything())
stu08 <- file4 %>% mutate(year = 2008) %>% select(year, everything())
tumo08 <- file5 %>% mutate(year = 2008) %>% select(year, everything())

#data cleansing

#is.na(file1)
#any(is.na(file1))
#colnames(file1)[colSums(is.na(file1)) > 0]
#names(which(sapply(file1, anyNA)))
#glimpse(file1)

#Reading in data for successive years - check headers/lists etc.

#YEAR 2009
setwd("~/DSAP/DATA - Swansea University contract/data09")

filenames <- list.files(path = ".",pattern = ".CSV")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en
colnames(file2) <- header_lo
colnames(file3) <- header_mo
colnames(file4) <- header_stu
colnames(file5) <- header_tumo
en09 <- file1 %>% mutate(year = 2009) %>% select(year, everything())
lo09 <- file2 %>% mutate(year = 2009) %>% select(year, everything())
mo09 <- file3 %>% mutate(year = 2009) %>% select(year, everything())
stu09 <- file4 %>% mutate(year = 2009) %>% select(year, everything())
tumo09 <- file5 %>% mutate(year = 2009) %>% select(year, everything())

#YEAR 2010
setwd("~/DSAP/DATA - Swansea University contract/data10")

filenames <- list.files(path = ".",pattern = ".CSV")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en
colnames(file2) <- header_lo
colnames(file3) <- header_mo
colnames(file4) <- header_stu
colnames(file5) <- header_tumo
en10 <- file1 %>% mutate(year = 2010) %>% select(year, everything())
lo10 <- file2 %>% mutate(year = 2010) %>% select(year, everything())
mo10 <- file3 %>% mutate(year = 2010) %>% select(year, everything())
stu10 <- file4 %>% mutate(year = 2010) %>% select(year, everything())
tumo10 <- file5 %>% mutate(year = 2010) %>% select(year, everything())

#YEAR 2011
setwd("~/DSAP/DATA - Swansea University contract/data11")

filenames <- list.files(path = ".",pattern = ".csv")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en
colnames(file2) <- header_lo
colnames(file3) <- header_mo
colnames(file4) <- header_stu
colnames(file5) <- header_tumo
en11 <- file1 %>% mutate(year = 2011) %>% select(year, everything())
lo11 <- file2 %>% mutate(year = 2011) %>% select(year, everything())
mo11 <- file3 %>% mutate(year = 2011) %>% select(year, everything())
stu11 <- file4 %>% mutate(year = 2011) %>% select(year, everything())
tumo11 <- file5 %>% mutate(year = 2011) %>% select(year, everything())

#YEAR 2012
setwd("~/DSAP/DATA - Swansea University contract/data12")

filenames <- list.files(path = ".",pattern = ".csv")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en1
colnames(file2) <- header_entry
colnames(file3) <- header_lo
colnames(file4) <- header_mo
colnames(file5) <- header_stu1
colnames(file6) <- header_tumo
en12 <- file1 %>% mutate(year = 2012) %>% select(year, everything())
entry12 <- file2 %>% mutate(year = 2012) %>% select(year, everything())
lo12 <- file3 %>% mutate(year = 2012) %>% select(year, everything())
mo12 <- file4 %>% mutate(year = 2012) %>% select(year, everything())
stu12 <- file5 %>% mutate(year = 2012) %>% select(year, everything())
tumo12 <- file6 %>% mutate(year = 2012) %>% select(year, everything())

#YEAR 2013
setwd("~/DSAP/DATA - Swansea University contract/data13")

filenames <- list.files(path = ".",pattern = ".csv")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en1
colnames(file2) <- header_entry
colnames(file3) <- header_lo
colnames(file4) <- header_mo
colnames(file5) <- header_stu1
colnames(file6) <- header_tumo
en13 <- file1 %>% mutate(year = 2013) %>% select(year, everything())
entry13 <- file2 %>% mutate(year = 2013) %>% select(year, everything())
lo13 <- file3 %>% mutate(year = 2013) %>% select(year, everything())
mo13 <- file4 %>% mutate(year = 2013) %>% select(year, everything())
stu13 <- file5 %>% mutate(year = 2013) %>% select(year, everything())
tumo13 <- file6 %>% mutate(year = 2013) %>% select(year, everything())

#YEAR 2014
setwd("~/DSAP/DATA - Swansea University contract/data14")

filenames <- list.files(path = ".",pattern = ".csv")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en1
colnames(file2) <- header_entry
colnames(file3) <- header_lo
colnames(file4) <- header_mo
colnames(file5) <- header_stu1
colnames(file6) <- header_tumo
en14 <- file1 %>% mutate(year = 2014) %>% select(year, everything())
entry14 <- file2 %>% mutate(year = 2014) %>% select(year, everything())
lo14 <- file3 %>% mutate(year = 2014) %>% select(year, everything())
mo14 <- file4 %>% mutate(year = 2014) %>% select(year, everything())
stu14 <- file5 %>% mutate(year = 2014) %>% select(year, everything())
tumo14 <- file6 %>% mutate(year = 2014) %>% select(year, everything())

#YEAR 2015
setwd("~/DSAP/DATA - Swansea University contract/data15")

filenames <- list.files(path = ".",pattern = ".CSV")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en1
colnames(file2) <- header_entry
colnames(file3) <- header_lo
colnames(file4) <- header_mo
colnames(file5) <- header_stu
colnames(file6) <- header_tumo
en15 <- file1 %>% mutate(year = 2015) %>% select(year, everything())
entry15 <- file2 %>% mutate(year = 2015) %>% select(year, everything())
lo15 <- file3 %>% mutate(year = 2015) %>% select(year, everything())
mo15 <- file4 %>% mutate(year = 2015) %>% select(year, everything())
stu15 <- file5 %>% mutate(year = 2015) %>% select(year, everything())
tumo15 <- file6 %>% mutate(year = 2015) %>% select(year, everything())

#YEAR 2016
setwd("~/DSAP/DATA - Swansea University contract/data16")

filenames <- list.files(path = ".",pattern = ".CSV")

for (i in 1:length(filenames)){
  oname = paste("file", i, sep = "")
  assign(oname, read.csv(filenames[i],header = FALSE,na.strings = ""))#(paste(oname, ".CSV", sep = "")))
}

colnames(file1) <- header_en1
colnames(file2) <- header_entry
colnames(file3) <- header_lo
colnames(file4) <- header_mo
colnames(file5) <- header_stu
colnames(file6) <- header_tumo
en16 <- file1 %>% mutate(year = 2016) %>% select(year, everything())
entry16 <- file2 %>% mutate(year = 2016) %>% select(year, everything())
lo16 <- file3 %>% mutate(year = 2016) %>% select(year, everything())
mo16 <- file4 %>% mutate(year = 2016) %>% select(year, everything())
stu16 <- file5 %>% mutate(year = 2016) %>% select(year, everything())
tumo16 <- file6 %>% mutate(year = 2016) %>% select(year, everything())

#remove unnecessary columns in files with differing variables ready for rbind
en12 <- select(en12, year:mode_study)
en13 <- select(en13, year:mode_study)
en14 <- select(en14, year:mode_study)
en15 <- select(en15, year:mode_study)
en16 <- select(en16, year:mode_study)
stu12 <- select(stu12, year:postcode)
stu13 <- select(stu13, year:postcode)
stu14 <- select(stu14, year:postcode)

#bind rows to create all years data frames
all_en <- bind_rows(en08, en09, en10, en11, en12, en13, en14, en15, en16)
all_entry <- bind_rows(entry12, entry13, entry14, entry15, entry16)
all_lo <- bind_rows(lo08, lo09, lo10, lo11, lo12, lo13, lo14, lo15, lo16)
all_mo <- bind_rows(mo08, mo09, mo10, mo11, mo12, mo13, mo14, mo15, mo16)
all_stu <- bind_rows(stu08, stu09, stu10, stu11, stu12, stu13, stu14, stu15, stu16)
all_tumo <- bind_rows(tumo08, tumo09, tumo10, tumo11, tumo12, tumo13, tumo14, tumo15, tumo16)

glimpse(all_en)

#provide unique identifier for merging data frames
all_en <- all_en %>% mutate(combi = paste0(year, student_id, module_id))
all_entry <- all_entry %>% mutate(combi = paste0(year, student_id, module_id))

#merge enrolment and entry
all_ent <- all_en %>% left_join(all_entry, by = "combi")

#unique identifier for bringing in student info
all_ent <- all_ent %>% mutate(combi2 = paste0(year.x, student_id.x))
all_stu <- all_stu %>% mutate(combi2 = paste0(year, student_id))

#merge enrolment and student
all_sid <- all_ent %>% left_join(all_stu, by = "combi2")

#remove na
all_sid <- all_sid %>% select_if(~sum(!is.na(.)) > 0)

#now we need to do the same to the module, location and tutor information...
#make the unique id to combine on
all_mo <- all_mo %>% mutate(combi3 = paste0(year, location_code))
all_lo <- all_lo %>% mutate(combi3 = paste0(year, location_code))

#merge module and location
all_molo <- all_mo %>% left_join(all_lo, by = "combi3")

#unique identifier for bringing in tutor info
all_molo <- all_molo %>% mutate(combi4 = paste0(year.x, module_id))
all_tumo <- all_tumo %>% mutate(combi4 = paste0(year, module_id))

#merge module and tutor
all_molotumo <- all_molo %>% left_join(all_tumo, by = "combi4")

#ultimately we will get the information from the modular file into the student file
#producing one file with all valid variables before linking to outcomes
#therefore, as before, we need an identifier to merge on

all_sid <- all_sid %>% mutate(combi4 = paste0(year.x, module_id.x))

#this identifier (combi4) is already in molotumo so we can merge the two files
stu.rol <- all_sid %>% left_join(all_molotumo, by = "combi4")

#remove NA filled columns
stu.rol <- stu.rol %>% select_if(~sum(!is.na(.)) > 0)
#remove surperflous columns
student.enrolment <- stu.rol %>% select(-ends_with("y.x"), -ends_with("x.y"), -ends_with("y.y"),
                               -ends_with("x.x.x"), -ends_with("y.y.y"))

#set working directory for outcome data read in
setwd("~/DSAP")

#read in hard outcome file
accreditation <- read.csv("AllAccreditation.csv")

#we're going to leave outcomes for now and have a look at reducing variables
#before doing some EDA
sv <- select(student.enrolment, c(1, 5:6, 8, 10, 12:14, 18, 24:28, 30, 33, 43:44, 46:47, 51, 57:58, 60:65, 71:72, 74))
glimpse(sv)
#we need to 'unite' out highest qual columns - one contains NA up til 2011, the other NA beyond 2011
sv <- unite(sv, highest_qual, highest_qual.x, highest_qual.y, sep = "", remove = TRUE)
#the united column contains NA's, we remove these using 'gsub'
sv$highest_qual <- gsub("NA", "", sv$highest_qual)
#additionally this column needs tidying - there are two sets of coded quals, pre and post 2011
unique(sv$highest_qual)
sv$highest_qual <- gsub("H80", "Degree/ProfQual", sv$highest_qual)
sv$highest_qual <- gsub("C80", "Degree/ProfQual", sv$highest_qual)
sv$highest_qual <- gsub("28", "Degree/ProfQual", sv$highest_qual)
sv$highest_qual <- gsub("P80", "Level3", sv$highest_qual)
sv$highest_qual <- gsub("40", "Level3", sv$highest_qual)
sv$highest_qual <- gsub("Q80", "Level2", sv$highest_qual)
sv$highest_qual <- gsub("55", "Level2", sv$highest_qual)
sv$highest_qual <- gsub("R80", "Level1", sv$highest_qual)
sv$highest_qual <- gsub("56", "Level1", sv$highest_qual)
sv$highest_qual <- gsub("X06", "Unknown", sv$highest_qual)
sv$highest_qual <- gsub("99", "Unknown", sv$highest_qual)
unique(sv$highest_qual)

#....and welsh speaker
sv$welsh_speaker <- gsub("1", "fluent", sv$welsh_speaker)
sv$welsh_speaker <- gsub("2", "non-fluent", sv$welsh_speaker)
sv$welsh_speaker <- gsub("3", "non-speaker", sv$welsh_speaker)
sv$welsh_speaker <- gsub("9", "unknown", sv$welsh_speaker)

#employment status would be good too but the coding doesn't match the guidance
#coding received at last....
sv$employment_status <- gsub("21", "retired", sv$employment_status)
sv$employment_status <- gsub("19", "employedLE", sv$employment_status)
sv$employment_status <- gsub("18", "unemployed", sv$employment_status)
sv$employment_status <- gsub("16", "other", sv$employment_status)
sv$employment_status <- gsub("15", "self-employed", sv$employment_status)
sv$employment_status <- gsub("11", "student", sv$employment_status)
sv$employment_status <- gsub("3", "employedPT", sv$employment_status)
sv$employment_status <- gsub("1", "employedFT", sv$employment_status)

#rename columns
sv <- rename(sv, year = year.x.x)
sv <- rename(sv, centre_postcode = postcode)
sv <- rename(sv, postcode = postcode.y)

#clean global environment with 'gdata' and 'keep'
library(gdata)
keep(accreditation, student.enrolment, sv, sure = TRUE)

#make dates into dates and determine age at enrolment from enrolment date and dob
library(lubridate)
sv$enrolment_date <- dmy(sv$enrolment_date)
#lubridate has a problem with abbreviated year assuming it to be 2000+
#the following line introduces 19 - we can safely assume all adult learners in the
#dataset were born before 2000
sv$dob <- as.Date(format(as.Date(sv$dob, format = "%d-%b-%y"), "19%y%m%d"), "%Y%m%d")

#age determination
sv <- sv %>% mutate(age = (dob %--% enrolment_date) %/% years(1))

#what's the basic make-up of our adult learners
summary(sv$age)
sv %>% count(gender)

#enrolment_status is our dependent variable - we need to factorise it to be 1 and 0
#enrolled, pre-enrolled and transferred can all be classed as 1, withdrawn then 0

#we can create things in r and then assign them to our data frame

# Make an empty list/vector
empty_col <- c()
# Make a vector of the levels we want to assign the same value to (1 in this case)
crit_enrlmt <- c("ENROLLED", "TRANSFERRED", "PRE-ENROLLED")

# then write our if_else loop
for (i in 1:length(sv$enrolment_status)){ 
  if (sv$enrolment_status[i] %in% crit_enrlmt){
    empty_col[i] <- 1
  } 
  else { 
    empty_col[i] <- 0
  } 
}   

# Assign a name to the empty_col vector and add it to the 'sv' dataframe
sv$numeric_en <- empty_col

#we need to turn some of our character strings to factors
sv$enrolment_status <- as.factor(sv$enrolment_status)
sv$employment_status <- as.factor(sv$employment_status)
sv$withdrawal_reason <- as.factor(sv$withdrawal_reason)
sv$withdrawal_code <- as.factor(sv$withdrawal_code)
sv$highest_qual <- as.factor(sv$highest_qual)
sv$welsh_speaker <- as.factor(sv$welsh_speaker)
sv$disability <- as.factor(sv$disability)
sv$ethnicity <- as.factor(sv$ethnicity)
sv$deceased <- as.factor(sv$deceased)
sv$module_mode_study <- as.factor(sv$module_mode_study)
sv$taught_mon <- as.factor(sv$taught_mon)
sv$taught_tue <- as.factor(sv$taught_tue)
sv$taught_wed <- as.factor(sv$taught_wed)
sv$taught_thu <- as.factor(sv$taught_thu)
sv$taught_fri <- as.factor(sv$taught_fri)
sv$taught_sat <- as.factor(sv$taught_sat)
sv$tutor_id <- as.factor(sv$tutor_id)
sv$numeric_en <- as.factor(sv$numeric_en)

# ...take a look 
glimpse(sv) 

#select training and testing datasets taking a random sample of 75% for our train set
smp_size <- floor(0.75 * nrow(sv))

#set seed to make partition reproducible
set.seed(123)
#sample the dataset to get 75% at random
train_ind <- sample(seq_len(nrow(sv)), size = smp_size)

#make train and test set
train <- sv[train_ind, ]
test <- sv[-train_ind, ]

#make models using glm (for logistic regression), adding variables in as you go
model <- glm(numeric_en ~ age, data = train, family = "binomial")
summary(model)

model2 <- glm(numeric_en ~ age + gender, data = train, family = "binomial")
summary(model2)

model3 <- glm(numeric_en ~ age + gender + employment_status, data = train, family = "binomial")
summary(model3) 

#random classification plot
#library(popbio)
#logi.hist.plot(train$age, train$numeric_en, boxp = FALSE, type = "hist", col = "gray")

model4 <- glm(numeric_en ~ age + gender + welsh_speaker, data = train, family = binomial)
summary(model4)

model5 <- glm(numeric_en ~ age + gender + welsh_speaker + natid1, data = train, family = binomial)
summary(model5)

model6 <- glm(numeric_en ~ age + gender + welsh_speaker + natid1 + contact_hours, data = train, family = binomial)
summary(model6)
#plot(model6)

#this model is the best so far perhaps - or at least has the lowest AIC

#we'll come back to modelling later but it might be an idea to have a look at a bit more feature engineering
#what's our age make-up?
ggplot(sv, aes(age))+
  geom_bar()+
  ggtitle("Age Distribution")

#who drops out?
ggplot(sv, aes(age, colour = numeric_en))+
  geom_histogram()+
  ggtitle("Age Distribution with Enrolment Status")

#ggplot(sv, aes(age, fill = numeric_en))+
 # geom_histogram(binwidth = 10)
#ggplot(sv, aes(age, colour = numeric_en))+
 # geom_freqpoly()

#that's age - what about employment status? Or nationality? or gender?
ggplot(sv, aes(employment_status, fill = numeric_en))+
  geom_histogram(stat = "count")+
  ggtitle("Employment Status Distribution")
ggplot(sv, aes(natid1, fill = numeric_en))+
  geom_bar()+
  ggtitle("Nationality Distribution")
ggplot(sv[!is.na(sv$gender),], aes(gender, fill = numeric_en))+
  geom_bar()+
  ggtitle("Gender Distribution")
ggplot(sv[!is.na(sv$gender),], aes(gender, fill = numeric_en))+
  geom_bar(position = "fill")+
  ggtitle("Gender Enrolment Proportionality")

#facet wrapping around age
ggplot(sv, aes(age, fill = numeric_en))+
  facet_wrap(~employment_status)+
  geom_histogram()+
  ggtitle("Age and Employment Status")
sv %>% filter(!is.na(gender)) %>% 
  ggplot(aes(gender, fill = numeric_en))+
  facet_wrap(~employment_status)+
  geom_bar()+
  ggtitle("Gender and Employment Status")

#but why do they drop out?

#ggplot(sv, aes(age, colour = withdrawal_reason))+
 # geom_freqpoly()

#let's filter out the enrolled
sv %>% filter(numeric_en == 0) %>% 
  ggplot(aes(age, colour = withdrawal_reason))+
  geom_freqpoly()+
  ggtitle("Frequency of Withdrawal Reason")

#sv %>% filter(numeric_en == 0) %>% count(withdrawal_reason, age) %>% 
 # ggplot(aes(age, withdrawal_reason))+
  #geom_tile(aes(fill = n))

#tile plot on withdrawals and nationality
sv %>% filter(numeric_en == 0) %>% count(natid1, age) %>% 
  ggplot(aes(age, natid1))+
  geom_tile(aes(fill = n))+
  ggtitle("Withdrawals, Age and Nationality")

#gender and age - what have we got
sv %>% filter(!is.na(gender)) %>% 
  ggplot(aes(age, fill = gender))+
  geom_bar()+
  ggtitle("Gender and Age Distribution")

#boxplot
ggplot(sv, aes(numeric_en, age))+
  geom_boxplot()+
  ggtitle("Enrolment Status and Average Age")

#ggplot(sv, aes(numeric_en, age))+
 # geom_violin()

#proportionality
ggplot(sv, aes(age, fill = numeric_en))+
  geom_histogram(binwidth = 10, position = "fill")+
  coord_cartesian(xlim = c(16,100))+
  ggtitle("Proportionality of Enrolment Status across Age")
#density
ggplot(sv, aes(age, fill = numeric_en))+
  geom_density(position = "fill")+
  coord_cartesian(xlim = c(16,100))+
  ggtitle("Density of Enrolment Status across Age")

#density for specific year
#ggplot(sv[sv$year == 2016,], aes(age, fill = numeric_en))+
 # geom_density(position = "fill")+
  #coord_cartesian(xlim = c(16,100))
#density for specific gender
#ggplot(sv[sv$gender == "M",], aes(age, fill = numeric_en))+
 # geom_density(position = "fill")+
  #coord_cartesian(xlim = c(16,100))

#density identified as welsh speaker, faceted around nationality
ggplot(sv, aes(welsh_speaker, fill = numeric_en))+
  facet_wrap(~natid1)+
  geom_bar()+
  ggtitle("Welsh Speaker and Nationality with Enrolment Status")

#Fee amount?
ggplot(sv, aes(fee_amount, fill = numeric_en))+
  geom_histogram(binwidth = 10)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("Withdrawals and Fees")

#highest qual?
ggplot(sv, aes(highest_qual, fill = numeric_en))+
  geom_bar()+
  ggtitle("Qualifications and Enrolment")

ggplot(sv, aes(fee_amount, fill = numeric_en))+
  geom_histogram(binwidth = 10)+
  coord_cartesian(xlim = c(0, 200))+
  facet_wrap(~highest_qual)+
  ggtitle("Withdrawals, Qualifications and Fees")

#tutors?
ggplot(sv, aes(tutor_id, fill = numeric_en))+
  geom_bar(position = "fill")+
  coord_flip()+
  ggtitle("Tutors and Withdrawals")


###------------------------------------###
#ZONING IN ON FEWER VARIABLES
#BUT POSSIBLY ADDING A COUPLE MORE FOR GEOSPATIAL ANALYSIS
###------------------------------------###


#it might be easier to graph with a smaller group of variables
sv1 <- sv %>% select(year, postcode, employment_status, fee_amount, withdrawal_reason, highest_qual, gender, welsh_speaker, natid1, contact_hours, centre_postcode, tutor_id, age, numeric_en)
#maybe some more streamlined plotting of sv1
#what relationships are worth looking at
ggplot(sv1, aes(welsh_speaker, fill = numeric_en))+
  geom_bar(position = "fill")
#as expected
ggplot(sv1, aes(contact_hours, fill = numeric_en))+
  geom_histogram()
#shorter courses perhaps have more buy in
ggplot(sv1, aes(centre_postcode, fill = numeric_en))+
  geom_bar(position = "fill")+
  coord_flip()
#where are these centres?????
#read in postcode csv file
uk.postcodes <- read_csv("ukpostcodes.csv")
uk.postcodes <- uk.postcodes %>% select(postcode:longitude)
###sv2 <- sv1 %>% left_join(uk.postcodes, by = "postcode")
#this is fine but we have some issues - our postcode column in sv1 is 
#full of lower case letters - this is case sensitive so we need to make these
#proper
sv1$postcode <- str_to_upper(sv1$postcode)
#and merge the info from uk.postcodes onto our sv1 df
sv2 <- sv1 %>% left_join(uk.postcodes, by = "postcode")
#uk.postcodes1 <- uk.postcodes %>% rename(centre_postcode = postcode)
sv3 <- sv2 %>% left_join(uk.postcodes, c("centre_postcode" = "postcode"))
#so long/lats have been read in and  the following determines the distance between points in km
library(geosphere)
sv3$travel_distance <- distHaversine(sv3[,15:16], sv3[,17:18])/1000
#what's our mean/median?
summary(sv3$travel_distance)

#plot this
sv3 %>% filter(!is.na(travel_distance)) %>% 
  ggplot(aes(travel_distance, fill = numeric_en))+
  geom_histogram()

#weird outliers
sv3 %>% filter(!is.na(travel_distance)) %>% arrange(travel_distance) %>% tail()

#restrict xlim
sv3 %>% filter(!is.na(travel_distance)) %>% 
  ggplot(aes(travel_distance, fill = numeric_en))+
  geom_histogram()+
  xlim(0,75)+
  ggtitle("Withdrawals and Travel Distance")

#quick pca on our numerical variables
pca <- sv %>% select(fee_amount, contact_hours, age, year)
pc <- prcomp(pca, scale = T)
plot(pc)
summary(pc)
plot(pc, type = 'l')

library(rgl)
#pcdf <- data.frame(pc$x[,1:3])
#plot3d(pcdf$PC1, pcdf$PC2, pcdf$PC3)

###---------------###
#mixed clustering with PCAmix - Steven H e-mail with k-means as well
###---------------###
library(PCAmixdata)
#reduce dataset but keep a mix
sv4pca <- sv1 %>% select(-year, -postcode, -withdrawal_reason, -centre_postcode, -numeric_en)
#split data into two parts - numeric/non-numeric
split <- splitmix(sv4pca)
X1 <- split$X.quanti
X2 <- split$X.quali

#remove columns with 0 variance [important later on in dimension reduction]
X1 <- X1[, sapply(X1, function(v) var(v, na.rm=TRUE)!=0)]
#remove columns with only one level (similar to above)
X2 <- X2[, sapply(X2, function(col) length(unique(col))) > 1]

#PCAmix function - read about but performs PCA on numeric and multiple corresponce analysis on categorical
res.pcamix <- PCAmix(X.quanti = X1, X.quali = X2, rename.level = T, graph = F)
res.pcamix
res.pcamix$eig

library(reshape2)

melted.loadings <- melt(res.pcamix$sqload)
p <- ggplot(data = melted.loadings, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
p <- p + labs(x = "Variable", y = "Dimension", title = "Variablie influence to principle component")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

set.seed(1)

wss <- 0
for (i in 1:15) {
  km.out <- kmeans(res.pcamix$scores, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}



## K-means Clustering

qplot(1:15, wss,
      xlab = "Number of Clusters",
      ylab = "Within clusters sum of squares",
      main = "Elbow plot of within cluster sum of squares",
      geom = c("point","path"))


# The K-means solution with 4 clusters was chosen as the optimal number of clusters based on the elbow plot.
#elbow plot may be better at 7 - maybe investigate this
# The numbers cluster size is demonstrated in the following plot.

kmeans.pca4<-kmeans(res.pcamix$scores, 4, nstart = 20)
#kmeans.pca5<-kmeans(res.pcamix$scores, 5, nstart = 20)
#kmeans.pca6<-kmeans(res.pcamix$scores, 6, nstart = 20)

X<-kmeans.pca4$cluster

X1<-as.data.frame(cbind(names(X), X))

#define palette for plotting
cpal <- c("#3B7A9E", "#0F8243", "#FF9933", "#D32F2F")

X2 <- X1 %>% group_by(X) %>% count(X)
ggplot(X2, aes(x= X, y = n))+ geom_bar(stat="identity") +
  scale_y_continuous("Count") +
  scale_fill_manual(name = "Cluster", values = cpal) +
  ggtitle("Count within Cluster") +
  xlab("Cluster")

#not quite sure what's going on in the next bit
df2 <- cbind(res.pcamix$scores, kmeans.pca4$cluster)
df2 <- as.data.frame(df2)
colnames(df2) <- c("dim1", "dim2", "dim3", "dim4", "dim5", "4c")


centers.pca4 <- kmeans.pca4$centers
#centers.pca5 <- kmeans.pca5$centers
#centers.pca6 <- kmeans.pca6$centers

centers.pca4long <- melt(centers.pca4, measure.vars = 1:1)
#centers.pca5long <- melt(centers.pca5, measure.vars = 1:1)
#centers.pca6long <- melt(centers.pca6, measure.vars = 1:1)
#cpal <- c("#3B7A9E", "#0F8243", "#FF9933", "#D32F2F")

ggplot(centers.pca4long, aes(x=Var2, y=value, color= as.factor(Var1))) +
  geom_bar(stat = "identity") +
  facet_wrap(~Var1) +
  coord_flip() +
  labs(color = "Cluster") +
  scale_colour_manual(values = cpal)

#unsure about the following
df.cluster <- cbind(sv4pca, df2$`4c`)
names(df.cluster)[names(df.cluster) == "df2$`4c`"] <- "Cluster"

# subset the data for each cluster
df.cluster1 <- df.cluster[df.cluster$Cluster == 1, ]
df.cluster2 <- df.cluster[df.cluster$Cluster == 2, ]
df.cluster3 <- df.cluster[df.cluster$Cluster == 3, ]
df.cluster4 <- df.cluster[df.cluster$Cluster == 4, ]

##Analysis 

## Levels ##
library(data.table)
df.Levels <- as.data.frame(res.pcamix$levels$contrib.pct)
setDT(df.Levels, keep.rownames = TRUE)
df.Levels <- tidyr::separate(df.Levels, rn, c("variable", "Level"), sep = "=", remove = TRUE)

## Dimension contribution plot
#what does this do? What is it asking for? Input 'x' is a variable from the dataset eg. gender
dimcontriplot <- function(x) {
  y <- dplyr::filter(df.Levels, variable == x)
  
  melted <- melt(y[,-1 ])
  p <- ggplot(data = melted, aes(x = Level, y = variable, fill=value)) + geom_tile()
  p<- p + labs(x = "Level", y = "Dimension", title = "Level pct contribution to each Dimension")
  p<- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(p)
}
dimcontriplot("gender")

## Variable level Distribution within cluster
#and this function? what does it do? what is the input 'i'?
vargraph <- function(i) {
   colnames(i) <-  c("Cluster", "Level", "Count")
   
  ggplot(i, aes(x = Level, y = Count, fill = as.factor(Cluster))) +
    geom_bar(stat = "identity") +
    facet_wrap(~Cluster) +
    scale_y_continuous("Count") +
    scale_fill_manual(name = "Cluster", values = cpal) +
    ggtitle("Distribution within Cluster") +
    coord_flip()
}
vargraph(centers.pca4long)
#but this produces the same plot as before - the facet wrap one

#we need to bind the cluster back onto our original dataset or bind the numeric_en column onto the cluster dataframe
cluster.sv <- cbind(df.cluster, sv$numeric_en)
sv.cluster <- cbind(sv, df.cluster$Cluster)

cluster.sv <- rename(cluster.sv, cluster = Cluster)
cluster.sv <- rename(cluster.sv, numeric_en = 'sv$numeric_en')

cluster.sv %>% group_by(cluster, numeric_en) %>% 
  summarise(
    count = n()
  ) %>% mutate(prop = count/sum(count))

cluster.sv %>% group_by(cluster, numeric_en) %>% 
  summarise(agegroup = mean(age))

cluster.sv %>% group_by(cluster, numeric_en, gender) %>% 
  summarise(gendercount = n())

employment <- cluster.sv %>% group_by(cluster, employment_status) %>% 
  summarise(
    count = n()
  ) %>% mutate(prop = count/sum(count))

ggplot(cluster.sv, aes(age, colour = cluster))+
  geom_freqpoly()

ggplot(cluster.sv, aes(fee_amount, colour = cluster))+
  geom_freqpoly(binwidth = 10)+
  coord_cartesian(xlim = c(0,100))

ggplot(cluster.sv, aes(age, fill = cluster))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~cluster)

ggplot(cluster.sv, aes(gender, fill = cluster))+
  geom_histogram(stat = "count")+
  facet_wrap(~cluster)

  


###----------------------###
#GEOSPATIAL
###----------------------###

library(leaflet)

pal <- colorFactor(c("red", "green"), levels = levels(sv3$numeric_en))

sv3 %>% filter(year == 2016) %>% 
  leaflet() %>% 
  addProviderTiles(providers$Thunderforest.Pioneer) %>% 
  addCircleMarkers(lng = ~longitude.x, lat = ~latitude.x, 
                   radius = 4, fillColor = ~pal(sv3$numeric_en), weight = 0.1,
                   label = paste(
                     "Enrolment:", sv3$numeric_en,
                     "Age:", sv3$age,
                     "Employment Status:", sv3$employment_status)) %>% 
  addCircles(lng = ~longitude.y, lat = ~latitude.y)

sv3 %>% 
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addMarkers(lng = ~longitude.x, lat = ~latitude.x,
             clusterOptions = markerClusterOptions())

sv3a <- sv3 %>%
  mutate(lng.jitter = jitter(longitude.x)) %>%
  mutate(lat.jitter = jitter(latitude.x))

sv3a %>% filter(year == 2016) %>% 
  leaflet() %>% 
  addProviderTiles(providers$Thunderforest.Pioneer) %>% 
  addCircleMarkers(lng = ~lng.jitter, lat = ~lat.jitter, 
                   radius = 2, fillColor = ~pal(sv3$numeric_en), weight = 0.1,
                   opacity = 0.2,
                   label = paste(
                     "Enrolment:", sv3$numeric_en,
                     "Age:", sv3$age,
                     "Employment Status:", sv3$employment_status)) %>% 
  addCircles(lng = ~longitude.y, lat = ~latitude.y)

#can we size the centre on the proportion of withdrawals
sv4 <- sv3 %>% group_by(centre_postcode, longitude.y, latitude.y, numeric_en) %>% 
  summarise(count = n()) %>% mutate(prop = count/sum(count)*100)
#we need to filter to 1 or 0 perhaps
sv4 %>% filter(numeric_en == 1) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~longitude.y, lat = ~latitude.y, radius = ~prop)
#can we colour on this proportion perhaps
sv5 <- sv4 %>% filter(numeric_en == 1)
#we can write a function to colour our enrolment proportions
getColor <- function(sv5) {
  sapply(sv5$prop, function(prop) {
    if(prop > 85) {
      "green"
    } else if(prop > 70) {
      "orange"
    } else {
      "red"
    } })
}
centreColour <- getColor(sv5)
sv5 %>% leaflet() %>% addTiles() %>% 
  addCircles(lng = ~longitude.y, lat = ~latitude.y, radius = ~prop, color = centreColour)
#so this is quite good - where do people withdraw more often  

############ RANDOM FOREST - RETURN TO SUPERVISED LEARNING

#adding predictions from our GLM model gave decimal rather than binary outcomes.
#Random Forest gives the expected binary output so this section returns to supervised learning
#but utilises the Random Forest algorithm
library(randomForest)
#we've got some NAs which RF doesn't like so we need to select what's predictive and remove NAs
sv4rf <- sv3 %>% select(-year, -postcode, -withdrawal_reason, -centre_postcode, 
                        -latitude.x, -latitude.y, -longitude.x, -longitude.y) %>% na.omit
#train and test datasets at random
smp_size2 <- floor(0.75 * nrow(sv4rf))
set.seed(123)
train_ind2 <- sample(seq_len(nrow(sv4rf)), size = smp_size2)
train.sv4rf <- sv4rf[train_ind2, ]
test.sv4rf <- sv4rf[-train_ind2, ]
#random forest algorithm
rf.train <- train.sv4rf %>% select(highest_qual, contact_hours, age, travel_distance, fee_amount)
rf.label <- train.sv4rf$numeric_en
set.seed(1234)
rf <- randomForest(x = rf.train, y = rf.label, importance = T)

library(modelr)
testpredict <- test.sv4rf %>% add_predictions(rf)
empty_col2 <- c()
for (i in 1:length(testpredict$pred)){
  if (testpredict$pred[i] %in% testpredict$numeric_en[i]){
    empty_col2[i] = 1
  }
  else {
    empty_col2[i] = 0
  }
}
testpredict <- cbind(testpredict, empty_col2)
testpredict %>% group_by(empty_col2) %>% summarise(n = n())

1699/(1699 + 357)
#giving us an accuraacy of 83%. Not bad but we can definitely improve this........