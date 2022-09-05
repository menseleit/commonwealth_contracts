library(readr)
library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)

set.seed(42)
setwd("~/Intern/Contracts")

a <- read_csv("2015_2016 FY - AUSTENDER CNS.csv")
b <- read_csv("2016-2017-australian-government-contract-data.csv")
c <- read_csv("ALL CNs at September 2016.csv")
d <- read_csv("AusTender - All CNs upto 31Dec2015.csv")
e <- read_csv("bc2097b7-8116-4e9d-9953-98813635892a.csv")
f <- read_csv("CNs Parent and Amendments 28082018.csv")
g <- read_csv("fb1a9116-c85d-4a0b-a4e3-c54eab79efaf.csv")
h <- read_excel("2019-20-australian-government-contract-data.xlsx")
i <- read_excel("20142015fy.xlsx")
j <- read_excel("cn-parent-and-amendments-30092019-ltr.xlsx")


#Make sure Column Names are the same

names(d)[12] <- "UNSPSC Title"
names(d)[25] <- "Supplier Suburb"
names(d)[25] <- "Supplier Address"
names(d)[29] <- "Supplier ABN"
names(d)[5] <- "Amendment Date"
names(e)[16] <- "UNSPSC Code"
names(e)[11] <- "Value"
names(f)[10] <- "Value"
names(f)[14] <- "UNSPSC Code"
names(i)[5] <- "Amendment Date"
names(i)[12] <- "UNSPSC Title"
names(i)[24] <- "Supplier Address"
names(i)[25] <- "Supplier Suburb"
names(i)[29] <- "Supplier ABN"

#Change data types so we can join data
sapply(a, class)

a$`Supplier ABN` <- as.character(a$`Supplier ABN`)
c$`Supplier ABN` <- as.character(c$`Supplier ABN`)
c$`Office Postcode` <- as.numeric(c$`Office Postcode`)
d$`Supplier ABN` <- as.character(d$`Supplier ABN`)
f$`Office Postcode` <- as.numeric(f$`Office Postcode`)
e$Value <- as.numeric(e$Value)
f$Value <- as.numeric(f$Value)

#Join all csv files
all_data <- a %>% full_join(b) %>% full_join(c) %>% full_join(d) %>% full_join(e) %>% full_join(f) %>% full_join(g) 

#Change class to merge excel files
all_data$`Publish Date`<- as.POSIXct(all_data$`Publish Date`, format= "%d/%m/%Y", tz= "UTC")
all_data$`Amendment Date`<- as.POSIXct(all_data$`Amendment Date`, format= "%d/%m/%Y %H:%M", tz= "UTC")
all_data$`Start Date`<- as.POSIXct(all_data$`Start Date`, format= "%d/%m/%Y", tz= "UTC")
all_data$`End Date`<- as.POSIXct(all_data$`End Date`, format= "%d/%m/%Y", tz= "UTC")
all_data$`Amendment Start Date`<- as.POSIXct(all_data$`Amendment Start Date`, format= "%d/%m/%Y", tz= "UTC")
all_data$`Amendments Value` <- as.numeric(all_data$`Amendments Value`)
h$`Supplier ABN` <- as.character(h$`Supplier ABN`)
i$`Office Postcode` <- as.numeric(i$`Office Postcode`)
i$`Supplier ABN` <- as.character(i$`Supplier ABN`)
j$`Amendment Date`<- as.POSIXct(j$`Amendment Date`, format= "%d/%m/%Y %H:%M", tz= "UTC")
j$`Amendment Start Date`<- as.POSIXct(j$`Amendment Start Date`, format= "%d/%m/%Y", tz= "UTC")
j$`Amendments Value` <- as.numeric(j$`Amendments Value`)

#Join all excel files
all_data <- all_data%>% full_join(h) %>% full_join(i) %>% full_join(j)
colSums(is.na(all_data))
#Remove missing values > 65%
all_data<-all_data[, which(colMeans(!is.na(all_data)) > 0.65)]
#Sum of missing values in all columns
colSums(is.na(all_data))
#Remove rows with > 50 % missing data
all_data<-all_data[which(rowMeans(!is.na(all_data)) > 0.50),]
#Remove office postcode as it's not relevant
all_data <- subset(all_data, select = -c(`Office Postcode`))

#Data Type
sapply(all_data, class)

all_data$`Procurement Method` <- as.factor(all_data$`Procurement Method`) 
all_data$`Confidentiality Contract Flag`<- revalue(all_data$`Confidentiality Contract Flag`, c("Yes" = 1))
all_data$`Confidentiality Contract Flag`<- revalue(all_data$`Confidentiality Contract Flag`, c("No" = 0))
all_data$`Confidentiality Outputs Flag` <- revalue(all_data$`Confidentiality Outputs Flag`, c("Yes" = 1))
all_data$`Confidentiality Outputs Flag` <- revalue(all_data$`Confidentiality Outputs Flag`, c("No" = 0))
all_data$`Supplier ABN Exempt` <- revalue(all_data$`Supplier ABN Exempt`, c("Yes" = 1))
all_data$`Supplier ABN Exempt` <- revalue(all_data$`Supplier ABN Exempt`, c("No" = 0))
all_data$`Consultancy Flag` <- revalue(all_data$`Consultancy Flag`, c("No" = 0))
all_data$`Consultancy Flag` <- revalue(all_data$`Consultancy Flag`, c("Yes" = 1))

#Make new column 
all_data$Address <- paste(all_data$`Supplier Suburb`, ",", all_data$`Supplier Address`, ",", all_data$`Supplier Postcode`)

#Remove old address columns
all_data <- subset(all_data, select = -c(`Supplier Suburb`, `Supplier Address`, `Supplier Postcode`))

#Fix Na/Replace with median
all_data$Value[is.na(all_data$Value)] <- median(all_data$Value, na.rm = T)

#Remove rows in dates - find location then remove
which(is.na(all_data$`Publish Date`), arr.ind = TRUE)
all_data <- all_data[-c(1713858, 2798582, 2952516, 2957923, 3141444, 3256424, 3299501,
                        2278018, 2278019, 2278020, 2278021, 2278022, 2278023, 2667318, 2756057, 3195062, 3328389,
                        3338552, 3370891),]

colSums(is.na(all_data))


#Replace remainder na with string
all_data[is.na(all_data)] <- "None"




#Removing characters - could not get all of them

all_data$Description <- gsub("<p>", "", as.character(all_data$Description))
all_data$Description <- gsub("</p>", "", as.character(all_data$Description))
all_data$Description <- gsub("<P>", "", as.character(all_data$Description))
all_data$Description <- gsub("</P>", "", as.character(all_data$Description))
all_data$Description <- gsub("<r>", "", as.character(all_data$Description))
all_data$Description <- gsub("</r>", "", as.character(all_data$Description))
all_data$Description <- gsub("\r\n", "", as.character(all_data$Description))
all_data$Description <- gsub("\r\\n", "", as.character(all_data$Description))
all_data$Description <- gsub("<SPAN>", "", as.character(all_data$Description))
all_data$Description <- gsub("</SPAN>", "", as.character(all_data$Description))

#Checking Outlier
summary(all_data$Value) # Min is $1. 
boxplot(all_data$Value) #Not good visualisation. Will plot instead
plot(all_data$Value)
table(all_data$Value) # 22 people has $1 as their value. Seems like a typo/error. Will remove as it is outlier to the rest of data and i believe it is an error/typo.

#looking at largest values 
data <- all_data[with(all_data, order(-Value)),]
data <- data [1:15,]
data$Value
#No outliers appearing on the upper end of the data.

#Remove Outlier - $1 - remove all rows with $1 as income. 
which(all_data$Value == '1', arr.ind = TRUE)
all_data <- all_data[-c(206948,  221529,  287645,  289034,  440780,  546038,  546039,  554503,  554506,  577963,
                        579053,  851645,  866225,  932327,  933716, 1085452, 1190732, 1190733, 1199202, 1199205,
                        1222670, 1223755),]
table(all_data$Value) #check

View(all_data)

#Save csv
write.csv(all_data,"C:\\Users\\maddieenseleit\\Desktop\\all_data.csv", row.names = FALSE)




