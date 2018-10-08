
library(lattice)
library(plyr)
library(dplyr)
library(ggplot2)
library(Rmisc)

#import data
service <- read.csv("C:/Users/adegbolayu/Desktop/service2.csv",
                    header = T,na.strings = c(" "))
service <- select(service, -X)

service1 <- service

#rename some column headers
names(service1)[7] <- "Requesttype#"
names(service1)[10] <- "Exportplaces"
names(service1)[12] <- "Lastupdate"
names(service1)[13] <- "RequestDate"
names(service1)[14] <- "Acknowledged"
names(service1)[16] <- "Closed"
names(service1)[17] <- "UsingMobile"

#change some variables to date format 

library(lubridate)
service1$RequestDate <- ymd_hms(service1$RequestDate, tz = "EST")
service1$Lastupdate <- ymd_hms(service1$Lastupdate, tz = "EST")


#Create new variabes for request date with weekdays, day number,year, and month for request
service1$Requestweekday <- weekdays(service1$RequestDate)
service1$requestdaynumber <- day(service1$RequestDate)
service1$requestyear <- year(service1$RequestDate)
service1$requestmonth <- months(service1$RequestDate)
service1$requestmonthnumber <- month(service1$RequestDate)
service1$Requestdatemonth <- substr(service1$RequestDate ,0,7)


library(zoo)
service1$Requestdatemonth <-  as.yearmon(service1$Requestdatemonth)

#convert category variable to factor
service1$Category <- as.factor(service1$Category)


#remove request to city office and GE utility from category variable
service1 <- subset(service1, Category !="Request to City Auditor's Office")
service1 <- subset(service1, Category !="Request to General Manager of Utilities")

#remove data for request year 2014
service1 <- subset(service1, requestyear !="2014")

#remove data for request date "2015-02-21 17:15:00" and "2015-03-24 11:33:00".
#These are outliers for request year 2015, and id were used here
service1 <- subset(service1, Id !="1497414")
service1 <- subset(service1, Id !="1550260")
service1 <- subset(service1, Id !="3809275")

#rename some variable in category to combine them
levels(service1$Category)[levels(service1$Category) == "Emergency - Traffic"] <- "Street Sign"
levels(service1$Category)[levels(service1$Category) == "Flooding"] <- "Drainage/Ditch Maintenance"
levels(service1$Category)[levels(service1$Category) == "Tree/Limbs (Private Property)"] <- "Tree/Limbs"
levels(service1$Category)[levels(service1$Category) == "Emergency - Flooding"] <- "Drainage/Ditch Maintenance"
levels(service1$Category)[levels(service1$Category) == "Emergency - Other"] <- "Other"
levels(service1$Category)[levels(service1$Category) == "Emergency - Tree or Brush Debris"] <- "Tree/Limbs"
levels(service1$Category)[levels(service1$Category) == "NA"] <- "Other"
levels(service1$Category)[levels(service1$Category) == "General Police Enforcement Issue"] <- "General Police Enforcement"

#create a new column with only district from exportplace column
District.1 <- function(Exportplaces) {
  
  if (length(grep("District 1", Exportplaces)) > 0){
    return ("1")
  } else if (length(grep("District 2", Exportplaces)) > 0) {
    return ("2")
  } else if (length(grep("District 3", Exportplaces)) > 0) {
    return ("3")
  } else if (length(grep("District 4", Exportplaces)) > 0) {
    return ("4")
  } else {
    return ("N/A")
  }
}
District <- NULL
for (i in 1:nrow(service1)) {
  District <-c(District,District.1(service1[i,"Exportplaces"]))
}
service1$District <-as.factor(District)

#Some service category were created later, so lets normalise the dataset
##group data by service request category with dplyr
categorygroup <- group_by(service1,Category)
category_counts <-  summarise(categorygroup, count = n())

#import and graph normalised data by category
category.1 <- read.csv("C:/Users/adegbolayu/Desktop/CATEGORY1.CSV",
                       header = T,na.strings = c(" "))

#graph categorie to check the most frequent and least frequent
ggplot(category.1) +
  geom_bar(aes(x = reorder(Category,count),y = count, fill = Category), stat = "identity") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,700)) + 
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(x = Category, y = count, label = count),
            position = position_dodge(width = 0),
            vjust = 0.5, size = 2, hjust = -0.1) + 
  xlab ("Request Categories") +
  ylab ("Number of Requests") +
  coord_flip()

#Graph normalized request per year
ggplot(category.1) +
  geom_bar(aes(x = reorder(Category,normalisedyear),y = normalisedyear, fill = Category), stat = "identity") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,250)) + 
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(x = Category, y = normalisedyear, label = normalisedyear),
            position = position_dodge(width = 0),
            vjust = 0.5, size = 2, hjust = -0.1) + 
  xlab ("Request Categories") +
  ylab ("Number of Requests") +
  coord_flip()

#subset and plot number of request for the hurricane season (sept 11 2017-Oct 9 2017)
Irma <- subset(service1, RequestDate >="2017-09-11 00:06:00" & RequestDate < "2017-10-10 10:37:00")
Mathew <- subset(service1, RequestDate >="2016-10-08 08:39:00" & RequestDate < "2016-10-16 15:59:00")
hurricane <- rbind(Mathew, Irma)

hurricanegroup <- group_by(hurricane,Category)
hurricanegroup_counts <-  summarise(hurricanegroup, count = n())

ggplot(hurricanegroup_counts) +
  geom_bar(aes(x = reorder(Category,count),y = count, fill = Category), stat = "identity") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,400)) + 
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(x = Category, y = count, label = count),
            position = position_dodge(width = 0),
            vjust = 0.5, size = 3, hjust = -0.1) + 
  xlab ("Request Categories") +
  ylab ("Number of Requests during Hurricanes") +
  coord_flip()
#There are 586 "other" in issue type. let check if some issue type are being repeated within "other" 
#or another can be categorized

#subset new data set with only issue type "other"
other.type <- subset(service1, Category == "Other",select = c(3,6,22))

# create new variable that pick up key words from description for issue type "other"
other.type$Description <- as.character(other.type$Description)
keywords <- function(Description) {
  
  if (length(grep("Water", Description)) > 0){
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("water", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("WATER", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("Leaked", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("leaking", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("leaked", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("Leaking", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("Leak", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("leak", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("Pipe", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("pipe", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("Sprinkler", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("sprinkler", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("Sprinklers", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("sprinklers", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("plumbing", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("pipes", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("wet", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("hydrant", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("can it be replaced", Description)) > 0) {
    return ("Water Leak/Broken Pipes")
  } else if (length(grep("claw", Description)) > 0) {
    return ("Claw Truck Required")
  } else if (length(grep("claw", Description)) > 0) {
    return ("Claw Truck Required")
  } else if (length(grep("mosquito", Description)) > 0) {
    return ("Mosquito Control")
  } else if (length(grep("panhandling", Description)) > 0) {
    return ("Hanhandling/Homeless")
  } else if (length(grep("Homeless", Description)) > 0) {
    return ("Hanhandling/Homeless")
  } else if (length(grep("panhandle", Description)) > 0) {
    return ("Hanhandling/Homeless")
  } else if (length(grep("homeless", Description)) > 0) {
    return ("Hanhandling/Homeless")
  } else if (length(grep("Panhandling", Description)) > 0) {
    return ("Hanhandling/Homeless")
  } else if (length(grep("Roadkill", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("roadkill", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Animal", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("animal", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("animals", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("armadillo", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("raccon", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Turkey", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("possum", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("termites", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("fleas", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("puppy", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("dead", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Dead", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Dog", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("dog", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("dogs", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Dogs", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Cats", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Cat", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("cat", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("cats", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("bird", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("puppy", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("rooster", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("Rooster", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("roosters", Description)) > 0) {
    return ("Animals/Pest Control")
  } else if (length(grep("parked", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("CAR", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("Parking", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("parking", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("trailers", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("garage", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("parking spots", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("Honda", Description)) > 0) {
    return ("Parking in Yard(Other Than Driveway")
  } else if (length(grep("waste", Description)) > 0) {
    return ("Trash/Debris")
  } else if (length(grep("Waste", Description)) > 0) {
    return ("Trash/Debris")
  } else if (length(grep("Garbage",Description )) > 0) {
    return ("Trash/Debris")
  } else if (length(grep("garbage",Description )) > 0) {
    return ("Trash/Debris")
  } else if (length(grep("clean", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("balcony", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("recycling", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("Recycling", Description)) > 0) {
    return ("Trash/Debris")   
  } else if (length(grep("trash", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("debris", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("dumpster", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("trashy", Description)) > 0) {
    return ("Trash/Debris") 
  } else if (length(grep("speed", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("Speed", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("speeding", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("Speeding", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("Traffic", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("speeders", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("Speeders", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("traffic", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("sign", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("sign", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("blocked", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("bus stops", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("pole", Description)) > 0) {
    return ("Traffic Regulation")
  } else if (length(grep("light", Description)) > 0) {
    return ("Light(Street/Public Property") 
  } else if (length(grep("Light", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("lights", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("Lights", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("lightted", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("Lighted", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("Lighting", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("lighting", Description)) > 0) {
    return ("Light(Street/Public Property") 
  } else if (length(grep("dark", Description)) > 0) {
    return ("Light(Street/Public Property")
  } else if (length(grep("power", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("Wire", Description)) > 0) {
    return ("Power Lines/Wire")  
  } else if (length(grep("wire", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("Wires", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("wires", Description)) > 0) {
    return ("Power Lines/Wire") 
  } else if (length(grep("lines", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("Lines", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("power", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("Power", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("cable", Description)) > 0) {
    return ("Power Lines/Wire") 
  } else if (length(grep("Cable", Description)) > 0) {
    return ("Power Lines/Wire")
  } else if (length(grep("Contractor", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("fences", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("construction", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("Construction", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("Constructions", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("fence", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("mold", Description)) > 0) {
    return ("General Code Issue") 
  } else if (length(grep("roof", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("landlord", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("appartment", Description)) > 0) {
    return ("General Code Issue")
  } else if (length(grep("Road", Description)) > 0) {
    return ("Road Repair")  
  } else if (length(grep("manhole", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("road", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("bumpy", Description)) > 0) {
    return ("Road Repair") 
  } else if (length(grep("roadway", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("Lines", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("pavement", Description)) > 0) {
    return ("Road Repair") 
  } else if (length(grep("curb", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("curbs", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("Sinkhole", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("construction", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("sink", Description)) > 0) {
    return ("Road Repair")
  } else if (length(grep("holes", Description)) > 0) {
    return ("Road Repair") 
  } else if (length(grep("drug", Description)) > 0) {
    return ("Drug Dealer") 
  } else if (length(grep("Drug", Description)) > 0) {
    return ("Drug Dealer") 
  } else if (length(grep("Limbs", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("Shrubs", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("palmetto", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("Tree", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("trees", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("tree", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("bushes", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("leaves", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("plants", Description)) > 0) {
    return ("Tree/Limbs")
  } else if (length(grep("loud", Description)) > 0) {
    return ("Noise Complaint")
  } else if (length(grep("heard", Description)) > 0) {
    return ("Noise Complaint")
  } else if (length(grep("music", Description)) > 0) {
    return ("Noise Complaint")
  } else if (length(grep("hear", Description)) > 0) {
    return ("Noise Complaint")
  } else if (length(grep("noises", Description)) > 0) {
    return ("Noise Complaint")
  } else if (length(grep("Noises", Description)) > 0) {
    return ("Noise Complaint")
  } else if (length(grep("Steet", Description)) > 0) {
    return (" Street Sweeping")
  } else if (length(grep("litter", Description)) > 0) {
    return ("Street Sweeping")
  } else if (length(grep("washout", Description)) > 0) {
    return ("Street Sweeping")
  } else if (length(grep("Washout", Description)) > 0) {
    return ("Street Sweeping") 
  } else if (length(grep("sand", Description)) > 0) {
    return ("Street Sweeping")
  } else if (length(grep("graffiti", Description)) > 0) {
    return ("Graffiti")
  } else if (length(grep("art", Description)) > 0) {
    return ("Graffiti")
  } else if (length(grep("signal", Description)) > 0) {
    return ("Traffic Signal") 
  } else if (length(grep("turn", Description)) > 0) {
    return ("Traffic Signal") 
  } else if (length(grep("light timing", Description)) > 0) {
    return ("Traffic Signal") 
  } else if (length(grep("turns", Description)) > 0) {
    return ("Traffic Signal") 
  } else if (length(grep("sign", Description)) > 0) {
    return ("Traffic Signal")   
  } else if (length(grep("2-way", Description)) > 0) {
    return ("Traffic Signal") 
  } else if (length(grep("business", Description)) > 0) {
    return ("Non license business/Garage Sale") 
  } else if (length(grep("garage sale", Description)) > 0) {
    return ("Non license business/Garage Sale") 
  } else if (length(grep("sold", Description)) > 0) {
    return ("Non license business/Garage Sale") 
  } else if (length(grep("crosswalks", Description)) > 0) {
    return ("Sidewalk")   
  } else if (length(grep("cross-walks", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("Crosswalks", Description)) > 0) {
    return ("Sidewalk") 
  } else if (length(grep("sidewalk", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("cross-walk", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("bollard", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("pedestrians", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("crossing", Description)) > 0) {
    return ("Sidewalk")
  } else {
    return ("No Clear Description")
  }
}
keywords1<- NULL
for (i in 1:nrow(other.type)) {
  keywords1 <-c(keywords1,keywords(other.type[i,"Description"]))
}
other.type$keywords1<-as.factor(keywords1)

#sort new created variables in descending order
keyword = group_by(other.type,keywords1)
keyword_count = summarise(keyword, count = n())

#plot issue type "other" new variables with ggplot
ggplot(keyword_count) + 
  geom_bar(
    aes(x = reorder(keywords1,count), y = count, fill = keywords1, group = keywords1), 
    stat='identity', position = 'dodge') +
  geom_text(aes(reorder(keywords1,count), y = count, label = count),
            position = position_dodge(width = 0),
            vjust = 0.5, size = 3, hjust = -0.1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +  
  theme(legend.position ="none", axis.text.x = element_text(angle = 48, hjust = 1)) + 
  xlab ("Keywords") +
  ylab ("Number of Requests")+
  coord_flip()# flip graph

#investigation "Genral code issue" keywords
GeneralCode <- subset(service1, Category == "General Code Issue",select = c(3,6))

GeneralCode$Description <- as.character(GeneralCode$Description)
keywords2 <- function(Description) {
  
  if (length(grep("mold", Description)) > 0){
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("Appliances", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("Mold", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("rental", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("landlord", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("owner", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("railing", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("wall", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("renters", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("property", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("Fence", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("rented", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("house", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("home", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("Home", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("Homes", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("HOME", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("Owner", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("Owners", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("owner", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("house", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("House", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("residence", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("fence", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("management", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("properties", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("Termites", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("manager", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("apartment", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("Renter", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("houses", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("Occupants", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("landlord's", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("abandoned", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("Abandoned", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("Apartments", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("leasing", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("residence", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("vegetation", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("occupants", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("land lords", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("residential", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("homeowner", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("Homeowner", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("residential", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("LANDLORD", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("over grown", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("overgrown", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("Grass", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("resident", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("tenant", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("tenants", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("address", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("grass", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("Plumbing", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("weeds", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("patio", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("residents", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("Rentor", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("occupied", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("PROPERTY", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("cable", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("building", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("complex", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("Pool", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("garbage", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")  
  } else if (length(grep("Garbage", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")   
  } else if (length(grep("Sofa", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")   
  } else if (length(grep("recycling", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("mosquito", Description)) > 0) {
    return ("Property Issue(Rental and Owned)")
  } else if (length(grep("mosquitos", Description)) > 0) {
    return ("Property Issue(Rental and Owned)") 
  } else if (length(grep("parking", Description)) > 0) {
    return ("Parking Issue")  
  } else if (length(grep("driveway", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("parked", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("vehicles", Description)) > 0) {
    return ("Parking Issue")  
  } else if (length(grep("park", Description)) > 0) {
    return ("Parking Issue")  
  } else if (length(grep("vehicle", Description)) > 0) {
    return ("Parking Issue")  
  } else if (length(grep("Lawn", Description)) > 0) {
    return ("Parking Issue")   
  } else if (length(grep("vehicle(s)", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("Motorcycle", Description)) > 0) {
    return ("Parking Issue")  
  } else if (length(grep("cars", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("Camping", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("camping", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("Parking", Description)) > 0) {
    return ("Parking Issue")     
  } else if (length(grep("trucks", Description)) > 0) {
    return ("Parking Issue")   
  } else if (length(grep("car/trailer", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("Trailers", Description)) > 0) {
    return ("Parking Issue")    
  } else if (length(grep("leak", Description)) > 0) {
    return ("Water Leak")      
  } else if (length(grep("leaking", Description)) > 0) {
    return ("Water Leak")    
  } else if (length(grep("Bathroom", Description)) > 0) {
    return ("Bad Smell")      
  } else if (length(grep("bathroom", Description)) > 0) {
    return ("Bad Smell")        
  } else if (length(grep("Smells", Description)) > 0) {
    return ("Bad Smell")  
  } else if (length(grep("construction", Description)) > 0) {
    return ("Construction Issue")
  } else if (length(grep("Construction", Description)) > 0) {
    return ("Construction")
  } else if (length(grep("eyesore", Description)) > 0) {
    return ("Construction")
  } else if (length(grep("renovation", Description)) > 0) {
    return ("Construction") 
  } else if (length(grep("remodeling", Description)) > 0) {
    return ("Construction") 
  } else if (length(grep("permit", Description)) > 0) {
    return ("Permit/license") 
  } else if (length(grep("permits", Description)) > 0) {
    return ("Permit/license") 
  } else if (length(grep("Permit", Description)) > 0) {
    return ("Permit/license")  
  } else if (length(grep("licensing", Description)) > 0) {
    return ("Permit/license")   
  } else if (length(grep("license", Description)) > 0) {
    return ("Permit/license")   
  } else if (length(grep("liquor", Description)) > 0) {
    return ("Permit/license")  
  } else if (length(grep("unlicensed", Description)) > 0) {
    return ("Permit/license")   
  } else if (length(grep("Intersection", Description)) > 0) {
    return ("Traffic Regulation") 
  } else if (length(grep("burning", Description)) > 0) {
    return ("Permits/license") 
  } else if (length(grep("right-of-way", Description)) > 0) {
    return ("Traffic Regulation") 
  } else if (length(grep("halfway", Description)) > 0) {
    return ("Traffic Regulation") 
  } else if (length(grep("speed", Description)) > 0) {
    return ("Traffic Regulation")   
  } else if (length(grep("traffic", Description)) > 0) {
    return ("Traffic Regulation") 
  } else if (length(grep("Noise", Description)) > 0) {
    return ("Noise Compliance") 
  } else if (length(grep("Noise", Description)) > 0) {
    return ("Noise Compliance") 
  } else if (length(grep("dog", Description)) > 0) {
    return ("Noise Compliance") 
  } else if (length(grep("dogs", Description)) > 0) {
    return ("Noise Compliance") 
  } else if (length(grep("noise", Description)) > 0) {
    return ("Noise Compliance") 
  } else if (length(grep("Dogs", Description)) > 0) {
    return ("Noise Compliance")  
  } else if (length(grep("loud", Description)) > 0) {
    return ("Noise Compliance")
  } else if (length(grep("loud", Description)) > 0) {
    return ("Noise Compliance")  
  } else if (length(grep("party", Description)) > 0) {
    return ("Noise Compliance")  
  } else if (length(grep("music", Description)) > 0) {
    return ("Noise Compliance")   
  } else if (length(grep("barks", Description)) > 0) {
    return ("Noise Compliance")   
  } else if (length(grep("roosters", Description)) > 0) {
    return ("Noise Compliance")  
  } else if (length(grep("rooster", Description)) > 0) {
    return ("Noise Compliance")   
  } else if (length(grep("rooster", Description)) > 0) {
    return ("Noise Compliance")    
  } else if (length(grep("sidewalk", Description)) > 0) {
    return ("Sidewalk")   
  } else if (length(grep("curb", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("blocked", Description)) > 0) {
    return ("Sidewalk")
  } else if (length(grep("sign", Description)) > 0) {
    return ("Sign Issue")
  } else if (length(grep("signs", Description)) > 0) {
    return ("Sign Issue")  
  } else if (length(grep("signage", Description)) > 0) {
    return ("Sign Issue")   
  } else if (length(grep("Sign", Description)) > 0) {
    return ("Sign Issue")  
  } else if (length(grep("banner", Description)) > 0) {
    return ("Sign Issue")  
  } else if (length(grep("advertisements", Description)) > 0) {
    return ("Sign Issue") 
  } else if (length(grep("Tree", Description)) > 0) {
    return ("Tree/Debris")
  } else if (length(grep("Trees", Description)) > 0) {
    return ("Tree/Debris")  
  } else if (length(grep("debris", Description)) > 0) {
    return ("Tree/Debris")   
  } else if (length(grep("oaks", Description)) > 0) {
    return ("Tree/Debris")   
  } else if (length(grep("trees", Description)) > 0) {
    return ("Tree/Debris")   
  } else if (length(grep("panhandling", Description)) > 0) {
    return ("Panhandling/Camping")  
  } else if (length(grep("Panhandling", Description)) > 0) {
    return ("Panhandling/Camping")   
  } else if (length(grep("camp", Description)) > 0) {
    return ("Panhandling/Camping")  
  } else if (length(grep("camped", Description)) > 0) {
    return ("Panhandling/Camping")  
  } else if (length(grep("campers", Description)) > 0) {
    return ("Panhandling/Camping")  
  } else if (length(grep("Homeless", Description)) > 0) {
    return ("Panhandling/Camping")
  } else if (length(grep("pan", Description)) > 0) {
    return ("Panhandling/Camping")
  } else {
    return ("No Clear Description")
  }
}
keywords3<- NULL
for (i in 1:nrow(GeneralCode)) {
  keywords3 <-c(keywords3,keywords2(GeneralCode[i,"Description"]))
}
GeneralCode$keywords3<-as.factor(keywords3)

#sort new created variables in descending order
keyword4 = group_by(GeneralCode,keywords3)
keyword4_count = summarise(keyword4, count = n())

#plot issue type "other" new variables with ggplot
ggplot(keyword4_count) + 
  geom_bar(
    aes(x = reorder(keywords3,count), y = count, fill = keywords3, group = keywords3), 
    stat='identity', position = 'dodge') +
  scale_y_continuous(expand = c(0, 0), limits = c(0,400)) + 
  geom_text(aes(reorder(keywords3,count), y = count, label = count),
            position = position_dodge(width = 0),
            vjust = 0.5, size = 3, hjust = -0.1) + 
  theme(legend.position ="none", axis.text.x = element_text(angle = 48, hjust = 1)) + 
  xlab ("Categories") +
  ylab ("Number of Requests")+
  coord_flip()

#request by district
districtrequest <- group_by(service1,District)
districtrequest_count <- summarise(districtrequest, count = n()) 

ggplot(districtrequest_count) + 
  geom_bar(aes(x = District, y = count, group = District), 
           stat='identity', position = 'dodge', fill = "red") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2500)) +  
  theme(legend.position ="none") + 
  xlab ("Districts") +
  ylab ("Number of Requests")+
  labs(caption = "Number of Requests by District Since 2015") +#place title at the botton of the graph
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

#plot the request type/category by district
##reorder category
service2 <- subset(service1, Category == "Trash/Debris (Private Property)"| Category == "Road Repair"|
                     Category == "Tree/Limbs"|Category == "General Code Issue"| Category == "Vehicle (Abandoned/Non-Operational)")
service2$Category1 <- with(service2, reorder(Category, Category, function(x) +length(x)))

ggplot(service2, aes(Category1)) +
  geom_bar(aes(fill = District), col = "black") +
  scale_y_continuous(expand = c(0, 0)) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1))+
  ylab ("Number of Requests")+
  xlab("categories") + 
  labs(caption = "Top Five Request Categories by District") + #place title at the botton of the graph
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))+
  coord_flip()

# check if mobile is used more to submit request
service1$UsingMobile <- as.factor(service1$UsingMobile) 
mobile = group_by(service1, UsingMobile)
mobile_counts = summarise(mobile, count = n())

pie(mobile_counts$count, labels = mobile_counts$count, main = "Report Using Mobile", 
    col = rainbow(length(mobile_counts$count)))
legend("right", c("Yes","No"), cex = 1,
       fill = rainbow(length(mobile_counts$count)))


library('forecast')
library('tseries')
library(scales)
library(ggpmisc)# for regression line

#FORCAST USING ARIMA FOR THE MONTH OF OCTOBER NOVEMBER AND DECEMBER 2018
#group by year and month number  for forcast
library('forecast')
library('tseries')
library(scales)
#group by year and month number  for forcast
yearly = group_by(service1, requestyear,requestmonth, Requestdatemonth)
yearly_counts = summarise(yearly, count = n())


#plot with ggplot to see trend
ggplot(yearly_counts, aes(Requestdatemonth, count, group = "Requestdatemonth")) + 
  geom_line(col= "red", size = 1.5)+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  ylab("Request Count")+
  xlab("DATE") +
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1))

#boxplot by month to see trend again
yearly_counts$requestmonth <- factor(yearly_counts$requestmonth, levels = month.name)
yearly_counts$Requestmonth <-  order(yearly_counts$requestmonth)
boxplot (yearly_counts$count ~ yearly_counts$requestmonth, ylim = c(0,600))


#september 2017 is clearly an outlier, let clean the data more with tsclean and create new clean variale
count_ts <- ts(yearly_counts[, c("count")])
yearly_counts$clean_cnt <- tsclean(count_ts)

#plot the clean count variable with ggplot

ggplot(yearly_counts, aes(Requestdatemonth, clean_cnt, group ="Requestdatemonth")) + 
  geom_line(col= "red", size = 1.5) + 
  ylab("Request Count")+
  xlab("DATE")+
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1))

#boxplot clean variable by month to see again
boxplot(yearly_counts$clean_cnt ~ yearly_counts$requestmonth, ylim = c(0,600))

#order data
yearly_counts <- yearly_counts[order(yearly_counts$Requestdatemonth),]
max(yearly_counts$Requestdatemonth)
min(yearly_counts$Requestdatemonth)
#transform data to time serie
yearly_countsts <- ts(yearly_counts$clean_cnt, start = min(yearly_counts$Requestdatemonth),
                      end =  max(yearly_counts$Requestdatemonth), frequency = 12)
yearly_countsts
#take diff and the log to see if data is stationnary with those transformation. Stationary mean that the data is "stable" around the mean and the variance 
plot(diff(log(yearly_countsts))) 

#check with adf if data is really stationary
adf.test(diff(log(yearly_countsts))) # p value is significant so data is stationary

#PLOT acf and Pacf
par(mfrow = c(1,2)) 
Acf(diff(log(yearly_countsts)), main = (""), col = "red") #determine value of p=0
Pacf(diff(log(yearly_countsts)), main = (""), col = "red") #determine value of p=0
par(mfrow = c(1,1))    

#data stay within the blue line after the lag 0 for ACF, so our q=0, and it is also the same with the PACF(p=0)

#fit ARIMA
library(forecast)
fit <- auto.arima(log(yearly_countsts))# or use fit <- arima(log(yearly_countsts), c(0, 1, 0), seasonal = list(order = c(0, 1, 0), period = 12))
pred <- predict(fit, n.ahead = 3, se.fit = T)
pred1 <- 2.718^(pred$pred)
pred1
ts.plot(yearly_countsts,pred1, log = "y", lty = c(1,3), col = "blue")

#CREATE NEW DATA SET WITH PREDICTED NUMBER for 2018
#read data from excel file
library(readxl)
yearly_counts_1 <- read_excel("yearly_counts.xlsx")

yearly_counts_1 <- yearly_counts_1[order(yearly_counts_1$requestdatemonth),]# descending order of data by month

#Plot data with predicted value ggplot and add regression line
p3 <- ggplot(yearly_counts_1, aes(requestdatemonth, clean_cnt)) + 
  geom_line(col= "red", size = 1.5) + 
  xlab("Date")+ ylab("Requests")+
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  scale_x_datetime(breaks = yearly_counts_1$requestdatemonth,
                   labels = format(yearly_counts_1$requestdatemonth,"%b-%Y"))+
  geom_smooth(method='lm',formula= y~x)+
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1))+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) 

#geom bar per year
sumyearly = group_by(yearly_counts_1,requestyear)
sumyearly_count = summarise(sumyearly, count = sum(round(clean_cnt, digits = 0)))

p4 <- ggplot(sumyearly_count, aes(x = requestyear, y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "light blue", width = 0.5) +
  geom_text(aes(x = requestyear, y = count, label = count),
            position = position_dodge(width = 1), vjust = -0.3) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,2600)) + 
  theme_light(base_size = 12) + xlab("Years")  + ylab(" ")+
  theme(plot.title=element_text(size=10, hjust = 0.5)) 

multiplot(p3,p4, cols = 1)# put both graphs in 1 figures

#boxplot per month for data with projection
yearly_counts_1$requestmonth <- months.Date (yearly_counts_1$requestdatemonth)
yearly_counts_1$requestmonth <-  ordered(yearly_counts_1$requestmonth, month.name)
boxplot (yearly_counts_1$clean_cnt ~ yearly_counts_1$requestmonth, ylim = c(0,600))


