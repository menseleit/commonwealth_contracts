#Import Dataset
library(dplyr)
library(plyr)
library(readxl)
library(tibble)
library(janitor)

a1 <- read_excel("Documents/Intern/Commonwealth Contracts/all_agencies2021.xlsx")
a2 <- read_excel("Documents/Intern/Commonwealth Contracts/all_agencies2020_2021.xlsx")
a3 <- read_excel("Documents/Intern/Commonwealth Contracts/all_agencies2019_2020.xlsx")
#View Data 
# There is a hidden row in excel called 'Agency Ref ID'. Shows up in Rstudio. Unsure why it was hidden. 
# Remove first 9 rows as they are irrelevant.


#Remove select rows
a1 <- all_agencies2021[-c(1:9), ]
a2 <- all_agencies2020_2021[-c(1:9), ]
a3 <- all_agencies2019_2020[-c(1:9), ]
#Convert first row to header
colnames(a1) <- as.character(a1[1,])
colnames(a2) <- as.character(a2[1,])
colnames(a3) <- as.character(a3[1,])
a1 <- a1[-1,]
a2 <- a2[-1,]
a3 <- a3[-1,]
View(a3) #change to view all 3

#Merge
all_agencies1 <- a1 %>% full_join(a2)
all_agencies1 <- all_agencies1 %>% full_join(a3)
View(all_agencies1)

#Checking for row duplicates
table(duplicated(all_agencies1))

#Determine data types 
sapply(all_agencies1, class)
#All data types are characters as we changed them to replace the headers. 
all_agencies1$`Value (AUD)` <- as.numeric(all_agencies1$`Value (AUD)`)
#Checking Na count before changing type as there is little data.
colSums(is.na(all_agencies1))
sum(is.na(all_agencies1$`Confidentiality Reason(s) - Contract`)) #Remove column. More than 50% of data is missing
sum(is.na(all_agencies1$`Confidentiality Reason(s) - Outputs`)) #Remove column. More than 50% of data is missing
#Removing columns
all_agencies1= select(all_agencies1, -"Confidentiality Reason(s) - Contract")
all_agencies1 = select(all_agencies1, -"Confidentiality Reason(s) - Outputs")

#Replace Y/N with 0,1
all_agencies1$`Confidentiality - Contract` <- revalue(all_agencies1$`Confidentiality - Contract`, c("Y" = 1))
all_agencies1$`Confidentiality - Contract` <- revalue(all_agencies1$`Confidentiality - Contract`, c("N" = 0))
all_agencies1$`Confidentiality - Outputs` <- revalue(all_agencies1$`Confidentiality - Outputs`, c("Y" = 1))
all_agencies1$`Confidentiality - Outputs` <- revalue(all_agencies1$`Confidentiality - Outputs`, c("N" = 0))

#Converting date columns
all_agencies1$`Publish Date` <- excel_numeric_to_date(as.numeric(as.character(all_agencies1$`Publish Date`)), date_system = "modern")    
all_agencies1$`Start Date` <- excel_numeric_to_date(as.numeric(as.character(all_agencies1$`Start Date`)), date_system = "modern")
all_agencies1$`End Date` <- excel_numeric_to_date(as.numeric(as.character(all_agencies1$`End Date`)), date_system = "modern")

#Replacing missing values
colSums(is.na(all_agencies1)) # Agency Ref and Supplier ABN both contain missing values.

#replacing NAs with 'None' as the columns do not contain data necessary to the output. Only relevant for locating certain data. 
#Agency ref.ID has 1% Nas. Could fill up but it's an ID number so leaving it as None for now
all_agencies1[is.na(all_agencies1)] <- "None"

#Check/Deal with Outlier
table(all_agencies1$`Value (AUD)`)
data <- all_agencies1[with(all_agencies1, order(-`Value (AUD)`)),]
data <- data[1:15,]
data$`Value (AUD)`

View(all_agencies1)
#save as csv file
write.csv(all_agencies1,"C:\\Users\\maddieenseleit\\Documents\\all_agencies.csv", row.names = FALSE)


