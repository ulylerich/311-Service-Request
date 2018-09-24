service <- read.csv("C:/Users/adegbolayu/Desktop/311_Service_Requests.csv",header = T,na.strings = c(" "))

# Remove unwanted variables from data Set
service1 <- subset(service,select = c(2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18))

#check data set variable structure
str(service1)

#change format of data variable
service1$Issue.Type <- as.factor(service1$Issue.Type)
service1$Summary <- as.character(service1$Summary)
service1$Description <- as.character(service1$Description)
service1$Description <- as.character(service1$Description)
service1$Assignee.name <- as.character(service1$Assignee.name)
service1$Service.Request.Date <- as.character(service1$Service.Request.Date)
service1$Last.updated <- as.character(service1$Last.updated)
service1$Acknowledged <- as.character(service1$Acknowledged)
service1$Closed <- as.character(service1$Closed)

#change some variables to date format 
service1$Service.Request.Date <- as.POSIXct(service1$Service.Request.Date,tz = "",
                                            format= "%m/%d/%Y %I:%M:%S %p")
service1$Acknowledged <- as.POSIXct(service1$Acknowledged,tz = "",
                                    format= "%m/%d/%Y %I:%M:%S %p")
service1$Last.updated <- as.POSIXct(service1$Last.updated,tz = "",
                                    format= "%m/%d/%Y %I:%M:%S %p")
service1$Closed <- as.POSIXct(service1$Closed,tz = "",
                              format= "%m/%d/%Y %I:%M:%S %p")

#Create new variabes with weekdays, day number, and year for request
service1$resquestweekday <- weekdays(service1$Service.Request.Date)
service1$requestdaynumber <- day(service1$Service.Request.Date)
service1$request.year <- year(service1$Service.Request.Date)

#convert request.year to factor
service1$request.year <- as.factor(service1$request.year)

#Plot issue by year with ggplot
yearly = group_by(service1, request.year)
yearly_counts = summarise(yearly, count = n())

ggplot(yearly_counts, aes(x = request.year , y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "light blue", width = 0.5) +
  geom_text(aes(x = request.year, y = count, label = count),
            position = position_dodge(width = 1), vjust = -0.3) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,2500)) + 
  theme_light(base_size = 12) + xlab("Year") + ylab("Count of request") + 
  ggtitle("Request since 2015") + 
  theme(plot.title=element_text(size=16, hjust = 0.5)) 
#create a new variable with month 
service1$request.month <- month(service1$Service.Request.Date)

#subset request for only 2018 and only 2017
year2018 <-subset(service1, request.year == "2018")
year2017 <- subset(service1, request.year == "2017" & request.month < 9)

#combined the two data set
year2017.2018 <- rbind(year2018,year2017)

#plot issue by year for 2018 and 2017 until end of August
yearly1718 = group_by(year2017.2018, request.year)
yearly_counts1718 = summarise(yearly1718, count = n())

ggplot(yearly_counts1718, aes(x = request.year , y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "light blue", width = 0.5) +
  geom_text(aes(x = request.year, y = count, label = count),
            position = position_dodge(width = 1), vjust = -0.3) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,2500)) + 
  theme_light(base_size = 12) + xlab("Year") + ylab("Count of request") + 
  ggtitle("Request for 2018 and unil August 2017") + 
  theme(plot.title=element_text(size=16, hjust = 0.5)) 


# Sort issue type in decreasing order and plot with ggplot
library(dplyr)
library(ggplot2)

order.category <- sort(table(service1$Issue.Type), decreasing = FALSE)
service1$Issue.Type <- factor(service1$Issue.Type, levels = names(order.category))

ggplot(service1, aes(Issue.Type)) +
  geom_bar(aes(fill = Issue.Type)) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(limits = levels(service1$Issue.Type)) +
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab ("Issue Type") +
  ylab ("Number of Request") +
  coord_flip()



#There are 578 "other" in issue type. let check if some issue type are being repeated within "other" 
#or another can be categorized

#subset new data set with only issue type "other"
other.type <- subset(service1, Issue.Type == "Other",select = c(2,3,4))

# create new variable that pick up key words from description for issue type "other"
other.type$Description <- as.character(other.type$Description)
keywords <- function(Description) {
  
  if (length(grep("Water", Description)) > 0){
    return ("WATER")
  } else if (length(grep("water", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("WATER", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Leaked", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("leaking", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("leaked", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Leaking", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Leak", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("leak", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Pipe", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("pipe", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Sprinkler", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("sprinkler", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Sprinklers", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("sprinklers", Description)) > 0) {
    return ("WATER")
  } else if (length(grep("Roadkill", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("roadkill", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Animal", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("animal", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("dead", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Dead", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Dog", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("dog", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("dogs", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Dogs", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Cats", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Cat", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("cat", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("cats", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("bird", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("puppy", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("rooster", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("Rooster", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("roosters", Description)) > 0) {
    return ("ANIMALS")
  } else if (length(grep("waste", Description)) > 0) {
    return ("GARBAGE/WASTE")
  } else if (length(grep("Waste", Description)) > 0) {
    return ("GARBAGE/WASTE")
  } else if (length(grep("Garbage",Description )) > 0) {
    return ("GARBAGE/WASTE")
  } else if (length(grep("garbage",Description )) > 0) {
    return ("GARBAGE/WASTE")
  } else if (length(grep("clean", Description)) > 0) {
    return ("GARBAGE/WASTE") 
  } else if (length(grep("balcony", Description)) > 0) {
    return ("GARBAGE/WASTE") 
  } else if (length(grep("speed", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("Speed", Description)) > 0) {
    return ("TRAFFIC MANAGEMENT")
  } else if (length(grep("speeding", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("Speeding", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("Traffic", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("speeders", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("Speeders", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("traffic", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("sign", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("sign", Description)) > 0) {
    return ("TRAFFIC REGULATION")
  } else if (length(grep("light", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)") 
  } else if (length(grep("Light", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)")
  } else if (length(grep("lights", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)")
  } else if (length(grep("Lights", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)")
  } else if (length(grep("lightted", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY")
  } else if (length(grep("Lighted", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)")
  } else if (length(grep("Lighting", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)")
  } else if (length(grep("lighting", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)") 
  } else if (length(grep("dark", Description)) > 0) {
    return ("LIGHT(PUBLIC PROPERTY)")
  } else if (length(grep("power", Description)) > 0) {
    return ("POWER LINES/WIRE")
  } else if (length(grep("Wire", Description)) > 0) {
    return ("POWER LINES/WIRE")  
  } else if (length(grep("wire", Description)) > 0) {
    return ("POWER LINES/WIRE")
  } else if (length(grep("Wires", Description)) > 0) {
    return ("POWER LINES/WIRE")
  } else if (length(grep("wires", Description)) > 0) {
    return ("POWER LINES/WIRE") 
  } else if (length(grep("lines", Description)) > 0) {
    return ("POWER LINES/WIRE")
  } else if (length(grep("Lines", Description)) > 0) {
    return ("POWER LINES/WIRE")
  } else if (length(grep("cable", Description)) > 0) {
    return ("POWER LINES/WIRE") 
  } else if (length(grep("Cable", Description)) > 0) {
    return ("POWER LINES/WIRE")
  } else {
    return ("MAIN CATEGORY")
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
  geom_text(aes(x = keywords1, y = count, label = count, group = keywords1),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 2) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,300)) +  
  theme(legend.position ="none", axis.text.x = element_text(angle = 48, hjust = 1)) + 
  xlab ("Keywords") +
  ylab ("Number of Request") 

#convert day as a factor
service1$resquestweekday <- as.factor(service1$resquestweekday)

#plot number of resquest by day of the week with ggplot
Weekdays = group_by(service1,resquestweekday)
Weekdays_counts = summarise(Weekdays, count = n())

ggplot(Weekdays_counts, aes(x = reorder(resquestweekday, -count), y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "light blue", width = 0.5) +
  geom_text(aes(x = reorder(resquestweekday, -count), y = count, label = count),
            position = position_dodge(width = 1), vjust = -0.3) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1500)) + 
  theme_light(base_size = 12) + xlab("Days") + ylab("Count of request") + 
  ggtitle("Request per day of the week") + 
  theme(plot.title=element_text(size=16, hjust = 0.5)) 

#plot issue.type/category by day of the week with ggplot
ggplot(service1, aes(Issue.Type)) +
  geom_bar(aes(fill = resquestweekday), col = "black") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(limits = levels(service1$Issue.Type)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab ("Issue Type") +
  ylab ("Number of Request") +
  coord_flip()

#creating a new variable with time of the day
service1$resquest.time <- hour(service1$Service.Request.Date)

#plot issue type by time of the day
timedays = group_by(service1,resquest.time)
timedays_counts = summarise(timedays, count = n())

ggplot(timedays_counts, aes(x = resquest.time, y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "red", width = 0.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,500)) + 
  scale_x_continuous(breaks = c(0:23)) + 
  theme_light(base_size = 12) + xlab("Time of Day") + ylab("Count of request") + 
  ggtitle("Request per time of the day") + 
  theme(plot.title=element_text(size=16, hjust = 0.5)) 

#create a new variable with day of the month
service1$request.month <- months(as.Date(service1$Service.Request.Date))
service1$request.month <- as.factor(service1$request.month)

#Plot Number of request by month of the year
monthly = group_by(service1, request.month)
monthly_counts = summarise(monthly, count = n())
monthly_counts$request.month <- ordered(monthly_counts$request.month, month.name)

ggplot(monthly_counts, aes(x = request.month, y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "red", width = 0.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,900)) + 
  theme(legend.position ="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Month") + 
  ylab("Count of request") + 
  ggtitle("Request per month ") + 
  theme(plot.title=element_text(size=16, hjust = 0.5)) 


#plot issue type by year
ggplot(service1, aes(Issue.Type)) +
  geom_bar(aes(fill = request.year), col = "black") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(limits = levels(service1$Issue.Type)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab ("Issue Type") +
  ylab ("Number of Request") +
  coord_flip()

# request by month and year
ggplot(service1, aes(request.year)) +
  geom_bar(aes(fill = request.month), col = "black") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(limits = levels(service1$request.year)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab ("Year") +
  ylab ("Number of Request") 

service1$Reported.using.mobile. <- as.factor(service1$Reported.using.mobile.)

# check if mobile is used more to submit request
mobile = group_by(service1, Reported.using.mobile.)
mobile_counts = summarise(mobile, count = n())

pie(mobile_counts$count, labels = mobile_counts$count, main = "Report Using Mobile", 
    col = rainbow(length(mobile_counts$count)))
legend("right", c("Yes","No"), cex = 1,
       fill = rainbow(length(mobile_counts$count)))

