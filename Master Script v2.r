library(maps)
library(ggmap)
library(dplyr)
library(ggplot2)
library(zipcode)
library(lubridate)

### Read the dataset (Date Range: 11/28/15-11/26/16)
data = read.csv("MyLA311_Service_Request_Data_2016.csv")

#### Call vs Mobile app ratio by month to look at mobile app awareness
#App vs Phone call referrals, service type question for each input channel
#All Request Sources. Twitter should be more leveraged as it is low cost to maintain
data %>%
  group_by(RequestSource) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  ggplot(aes(x = reorder(RequestSource,-count), y = count, fill = count))+
  geom_bar(stat = "Identity")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17))+
  geom_text(aes(label = comma(count)), size = 5, vjust = -1)+
  ggtitle("Request Count by Request Source")+
  ylab("Count")+
  xlab("")+
  guides(fill = FALSE)

#Request Types overall
data %>%
  group_by(RequestType) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  ggplot(aes(x = reorder(RequestType, -count), y = count, label = count, fill = RequestType))+
  geom_bar(stat = "Identity", position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(size = 2.5, vjust = -1)+
  ggtitle("Request Types")+
  xlab("Request Types")+
  ylab("Count of Request Type")+
  guides(fill = FALSE)

#Request Types by Call
data %>%
  filter(RequestSource == "Call") %>%
  group_by(RequestType) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  ggplot(aes(x = reorder(RequestType, -count), y = count, fill = count))+
  geom_bar(stat = "Identity", position = position_dodge())+
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17))+
  geom_text(aes(label = comma(count)), size = 5, vjust = -1)+
  ggtitle("Request Count through Calls by Request Source")+
  ylab("Count")+
  xlab("")+
  guides(fill = FALSE)

#Request Types by Mobile App
#More Grafiti Removal and Homeless Encampment reported. Make sense sence people can simply pull out the phone and report
#"Other" go up and possibly because in the call the staff can better understand the situation and better categorize the Request
#Further promote the use of 311 app instead of waiting for an operator to answer calls
data %>%
  filter(RequestSource == "Mobile App" | RequestSource == "Self Service") %>%
  group_by(RequestType) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  ggplot(aes(x = reorder(RequestType, -count), y = count, fill = count))+
  geom_bar(stat = "Identity", position = position_dodge())+
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17))+
  geom_text(aes(label = comma(count)), size = 5, vjust = -1)+
  ggtitle("Request Count through Devices by Request Source")+
  ylab("Count")+
  xlab("")+
  guides(fill = FALSE)

#call vs Mobile app ratio by month to look at mobile app awareness
#Mobile app usage has slightly increased throughout the year. However, it is not enough 
#City gov has rolled out the mobile app in Feb 2013. It has been almost 4 years.

totcountbymonth = data %>%
  filter(RequestSource == "Call" | RequestSource == "Mobile App" | RequestSource == "Self Service") %>%
  group_by(month) %>%
  summarise(totalcount = n())

totcountbymonthrequestsrc = data %>%
  filter(RequestSource == "Call" | RequestSource == "Mobile App" | RequestSource == "Self Service") %>%
  group_by(month, RequestSource) %>%
  summarise(countbyrequestsource = n())

appawareness = merge(totcountbymonthrequestsrc, 
                     totcountbymonth, 
                     by.x = "month",
                     by.y = "month",
                     all.x = T)

# diff btw the Max and Min of mobile app usage throughout the year: 8038
appawareness %>%
  filter(RequestSource == "Mobile App") %>%
  summarise(max(countbyrequestsource)-min(countbyrequestsource))

appawareness %>%
  mutate(ratio = round(countbyrequestsource/totalcount,3)) %>%
  group_by(RequestSource) %>%
  ggplot(aes(x = month, y =ratio, fill = RequestSource, label = ratio)) +
  geom_bar(stat = "identity") +
  ggtitle("Request Source Breakdown by Month") +
  xlab("Month") +
  ylab("Ratio")+
  scale_fill_manual(values = c("red","blue4", "blue"))

#call vs Mobile app ratio by zipcode to look at mobile app awareness in diff regions

totcountbyzip = data %>%
  filter(RequestSource == "Call" | RequestSource == "Mobile App" | RequestSource == "Self Service") %>%
  group_by(ZipCode) %>%
  summarise(totalcount = n())

totcountbyziprequestsrc = data %>%
  filter(RequestSource == "Call" | RequestSource == "Mobile App" | RequestSource == "Self Service") %>%
  group_by(ZipCode, RequestSource) %>%
  summarise(countbyziprequestsrc = n())

appawarenesszip = merge(totcountbyziprequestsrc, 
                        totcountbyzip, 
                        by.x = "ZipCode",
                        by.y = "ZipCode",
                        all.x = T)



# With Total count of Call and App above 10000, plot the ratio of the mobile app
cityname = read.csv("lacityzipcode.csv")
str(appawarenesszip)
appawarenesszipname = merge(appawarenesszip, 
                            cityname, 
                            by.x = "ZipCode",
                            by.y = "zip",
                            all.x = T)

name = paste(appawarenesszipname$ZipCode, appawarenesszipname$primary_city, sep = "-" )
appawarenesszipname1 = cbind(appawarenesszipname, name)

az = appawarenesszipname1 %>%
  filter(ZipCode != "0") %>%
  filter(ZipCode != "") %>%
  filter(totalcount >= 10000) %>%
  mutate(ratio = round(countbyziprequestsrc/totalcount,3)) %>%
  group_by(RequestSource) %>%
  arrange(RequestSource, -ratio)

write.csv(az,"az.csv")
az = read.csv("az.csv")
az$ZipCode = as.factor(az$ZipCode)
lev = levels(az$ZipCode)
newlev = lev[c(14,4,13,20,1,17,16,19,25,5,11,12,21,23,10,6,15,9,7,18,24,22,8,3,2)]
az$ZipCode = factor(az$ZipCode, levels = newlev)

levels(az$name)

lev = levels(az$name)
newlev = lev[c(14,4,13,20,1,17,16,19,25,5,11,12,21,23,10,6,15,9,7,18,24,22,8,3,2)]
az$name = factor(az$name, levels = newlev)


ggplot(az, aes(x = name, y = ratio, fill = RequestSource, label = ratio)) +
  geom_bar(stat = "identity") +
  ggtitle("Request Source Breakdown by ZipCode") +
  xlab("ZipCode") +
  ylab("Ratio")+
  scale_fill_manual(values = c("red","blue4", "blue"))+
  scale_y_continuous(breaks = seq(0, 1.25, 0.25),
                     labels = seq(0, 1.25, 0.25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hmap = qmap("Los Angeles California", zoom = 10)

map = az %>%
  filter(ZipCode != "90004" | ZipCode != "90006")

hmap + 
  stat_bin2d(data = az, aes(x = longitude, y = latitude, color = ratio)) +
  guides(fill = FALSE)



#Overall trends in calls / apps - sums and counts (April)
#Heatmap by Hour of the day, by Day of the month
str(data)
data$CreatedDate = mdy_hms(data$CreatedDate)
data$weekday = weekdays(data$CreatedDate)
data$weekday = as.factor(data$weekday)
data$hour = hour(data$CreatedDate)
data$month = month(data$CreatedDate, label = T, abbr = F)
lev = levels(data$weekday)
newlev = lev[c(4,2,6,7,5,1,3)]
data$weekday = factor(data$weekday, levels = newlev)

levmon = levels(data$month)
newlevmon = levmon[c(12, 1:11)]
data$month = factor(data$month, levels = newlevmon)

# Call - Hour of the Day
data %>%
  filter(RequestSource == "Call") %>%
  group_by(weekday, hour) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = factor(hour), fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "White", high = "Red")+
  ggtitle("Hour of the Day through Calls")+
  xlab("Day")+
  ylab("Hour")+
  guides(fill = FALSE)

# Mobile App - Hour of the Day
data %>%
  filter(RequestSource == "Mobile App") %>%
  group_by(weekday, hour) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = factor(hour), fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "White", high = "Red")+
  ggtitle("Hour of the Day through Mobile App")+
  xlab("Day")+
  ylab("Hour")+
  guides(fill = FALSE)

# Self Service - Hour of the Day
data %>%
  filter(RequestSource == "Self Service") %>%
  group_by(weekday, hour) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = factor(hour), fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "White", high = "Red")+
  ggtitle("Hour of the Day through Self Service")+
  xlab("Day")+
  ylab("Hour")+
  guides(fill = FALSE)

# Bar Chart or line by Month: August has the most requests
data %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count, label = count, fill = count))+
  geom_bar(stat = "identity")+
  geom_line(group = 1)+
  scale_y_continuous(limits = c(0,100000))+
  geom_text(size = 2.5, vjust = -1) +
  xlab("Month")+
  ylab("Number of Requests per Month")+
  ggtitle("Number of Requests by Month")+
  scale_fill_gradient(high = "darkblue", low = "lightblue")+
  guides(fill = FALSE)

# See if a particular Request Source shows a spike in August or all of them show
# Call - Day of the Month
data %>%
  #filter(RequestSource == "Call") %>%
  group_by(month, weekday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = month, fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "White", high = "Red")+
  ggtitle("Day of the Month")+
  xlab("Day")+
  ylab("Month")+
  guides(fill = FALSE)

# Mobile App - Day of the Month
data %>%
  filter(RequestSource == "Mobile App") %>%
  group_by(month, weekday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = month, fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "White", high = "Red")+
  ggtitle("Day of the Month through Mobile App")+
  xlab("Day")+
  ylab("Month")

# Self Service - Day of the Month
data %>%
  filter(RequestSource == "Self Service") %>%
  group_by(month, weekday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = month, fill = count))+
  geom_tile()+
  scale_fill_gradient(low = "White", high = "Red")+
  ggtitle("Day of the Month through Self Service")+
  xlab("Day")+
  ylab("Month")


# What contributes to the spike in August? - Bulk Items Pickup
data %>%
  group_by(month, RequestType) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(RequestType, -count), y = count))+
  geom_bar(stat = "identity")+
  facet_wrap(~month)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Month")+
  ylab("Number of Requests per Month")+
  ggtitle("Number of Requests Monthly Breakdown - 2016")

### Request Count & Request Ratio Data by Zip Code
data_zip = data %>% group_by(ZipCode) %>% summarise(request_count = n())

population = read.csv("population.csv")
income = read.csv("income.csv")

data_zip = merge(data_zip, population,
                by.x = "ZipCode",
                by.y = "zip",
                all.x = 1)
data_zip = merge(data_zip, income,
                 by.x = "ZipCode",
                 by.y = "zip",
                 all.x = 1)

data_zip$rate = (data_zip$request_count/data_zip$pop)*100

###############################
#### Device Usage Analysis ####
###############################

data$month = month(mdy_hms(data$CreatedDate), label = 1)
data$year = year(mdy_hms(data$CreatedDate))
data$mobile = data$RequestSource == "Mobile App" | data$RequestSource == "Self Service"
data$app = data$RequestSource == "Mobile App"

# Time Series
data_appT = data %>% 
  filter(RequestSource == "Mobile App" | data$RequestSource == "Self Service" | 
           data$RequestSource == "Call") %>%
  group_by(month, year) %>% summarise(app_usage = sum(mobile), app_ratio = app_usage/n())

# Breakdown by Zip Code
data_appZ = data %>% 
  group_by(ZipCode) %>% summarise(app_usage = sum(mobile), app_ratio = app_usage/n())

# Breakdown by Request Type
data_appR = data %>% 
  filter(RequestSource == "Mobile App" | data$RequestSource == "Self Service" | 
           data$RequestSource == "Call") %>%
  group_by(RequestType) %>% summarise(app_usage = sum(mobile), app_ratio = app_usage/n(),
                                      request = n())

ggplot(data_appR, aes(x=reorder(RequestType, -app_ratio), y=app_ratio, fill = request))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  guides(fill = FALSE)+
  ggtitle("Device Usage (Mobile App + Self Service) Ratio by Request Type")+
  xlab("")+
  ylab("Device Usage Ratio")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17))

# Only Mobile App
data_appA = data %>% 
  filter(RequestSource == "Mobile App" | RequestSource == "Call") %>%
  group_by(RequestType) %>% summarise(app_usage = sum(app), app_ratio = app_usage/n(),
                                      request = n())

ggplot(data_appA, aes(x=reorder(RequestType, -app_ratio), y=app_ratio, fill = request))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(low = "white", high = "darkred")


## Service Type breakdowns. Which requests are most common over time and areas
## Geographic + Service Type breakdown
# over time
names(data)

unique(data$RequestType)

a=head(data,50)

data$RequestDate=mdy_hms(data$CreatedDate)

data=data%>%
  mutate(Weekday=wday(RequestDate,label=TRUE),Hour=hour(RequestDate))

save(data,file="data1-1.rda")

ggplot(data,aes(x=RequestType))+geom_bar()
levels(data$RequestType)
top=c("Bulky Items","Graffiti Removal","Metal/Household Appliances")
Bulky=data%>%
  filter(RequestType==top[1])
p1=Bulky%>%
  group_by(Weekday,Hour)%>%
  summarize(count=n())
ggplot(p1,aes(x=factor(Weekday),y=Hour,fill=count))+geom_tile()+
  scale_fill_gradient(high="Red",low="white")
Graffiti=data%>%
  filter(RequestType==top[2])
p2=Graffiti%>%
  group_by(Weekday,Hour)%>%
  summarize(count=n())
ggplot(p2,aes(x=factor(Weekday),y=Hour,fill=count))+geom_tile()+
  scale_fill_gradient(high="Red",low="white")
Metal=data%>%
  filter(RequestType==top[3])
p3=Metal%>%
  group_by(Weekday,Hour)%>%
  summarize(count=n())
ggplot(p3,aes(x=factor(Weekday),y=Hour,fill=count))+geom_tile()+
  scale_fill_gradient(high="Red",low="white")
############
#failed loop:check latter
for( i in 1:3){
  b[[i]]=data%>%
    filter(RequestType==top[i])%>%
    group_by(Weekday,Hour)%>%
    summarize(count=n())
  c[[i]]=ggplot(b[[i]],aes(x=factor(Weekday),y=Hour,fill=count))+geom_tile()+
    scale_fill_gradient(high="Red",low="white")
}

#over area
LA="Los Angeles"
LAMap=qmap(LA,maptype="road",color="bw")
LAMap+geom_point(data=Bulky,aes(x=Longitude,y=Latitude),alpha=0.01,color="red")




##########################################################################
####Regression part
###1 Request number vs poplulation, median age, income
age=read.csv("MedianAge.csv")
income=read.csv("income.csv")

data=data%>%
  mutate(date=mdy_hms(CreatedDate))%>%
  arrange(date)%>%
  filter(date < mdy("11/28/2016") & date>(mdy("11/28/2015")))
##output the data with right date range
write.csv(data,file="filtereddata.csv")
##dependent ready:with 140 zipcodes in total.
#############################################rerun regression part:from here
data=read.csv("filtereddata.csv")
request=data%>%
  group_by(ZipCode)%>%
  dplyr::summarize(request=n())
colnames(request)[1]="zip"
##independent ready
age=arrange(age,Zip.Code)
income=arrange(income,zip)
colnames(age)[1]="zip"
independent=merge(age,income,by.age=zip,by.income=zip)
independent=independent[,-(4:7)]
independent=independent[,-5]
##regression dataset ready
regression1=merge(request,independent,by.request=zip,by.independent=zip)
colnames(regression1)[c(3,4)]=c("population","age")

regression1=read.csv("regression1.csv")

##regression
##run twice. age is not significant here, so delete it in the second round
fit <- lm(request ~ population + income, data=regression1)
summary(fit) # show results



target=c(0,1)
regression1$intercept=fit[[1]][1]
regression1$betapop=fit[[1]][2]
regression1$betaincome=fit[[1]][3]
regression1=regression1%>%
  mutate(benchmark=intercept+betapop*population+betaincome*income)%>%
  mutate(target=target[1+(benchmark*0.95>request)])
ggplot(regression1,aes(x=zip,y=benchmark,group="aa"))+geom_line()
#####################################################plot with spatial dataset
##try to plot these by ZCTAS

library("tigris")

library(sp)

library(rgeos)

library(leaflet)
a=regression1$zip
b=regression1%>%
  filter(target==1)
b=b$zip
df=zctas(cb=FALSE,starts_with = a)
subdf=zctas(cb=FALSE,starts_with = b)
##exmaple given by the package creator
#uas <- urban_areas(cb = FALSE)
#la_ua <- uas[grep("Los Angeles--Long Beach--Anaheim", uas$NAME10), ]

leaflet(df) %>% addTiles() %>% addPolygons(fillOpacity = 0, color = "blue",weight=2)%>%
  addPolygons(data=subdf,fillOpacity = 0.7,color="pink")

####################################################testing
##try package choroplethr & choroplethrZip instead
##choroplethrZip is not on CRAN as it is too big. using devtool to connect github

library(devtools)
library(scales)
library(htmlTable)
install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)
library(choroplethr)
##plot


data(df_pop_zip)

zip_choropleth(df_pop_zip,zip_zoom=a,legend="Population")


#####nope, it's for common census data, with really really easy steps

##################2 ration of app use=request by app/ total request vs poplulation, median age, income
app=data%>%
  filter(RequestSource=="Mobile App")%>%
  group_by(ZipCode)%>%
  dplyr::summarise(count=n())
##only 126 obs left. some 14 areas don't have app request at all
colnames(app)[1]="zip"
regression2=merge(request,app,request.by=zip,app.by=zip)
regression2=regression2%>%
  mutate(ratio=count/request)

regression2=merge(regression2,independent,regression2.by=zip,independent.by=2)
colnames(regression2)[c(5,6)]=c("population","age")

fit <- lm(ratio ~ population+income + age , data=regression2)
summary(fit) # show results
#not very significant(overall:0.09797;0.03 and 0.1013) and r^2 is not good:0.03707

ggplot(regression2,aes(x=age,y=ratio))+geom_point()
####################################################ploting the population and ratio


ratio=regression1%>%
  mutate(requestdensity=request/population)%>%
  arrange(-requestdensity)
ratio=ratio[-1,]

ratio=ratio%>%
  mutate(order=(1:138))%>%
  mutate(group=ceiling(order/20))

ratio1=ratio$zip[ratio$group==1]
ratio2=ratio$zip[ratio$group==2]
ratio3=ratio$zip[ratio$group==3]
ratio4=ratio$zip[ratio$group==4]
ratio5=ratio$zip[ratio$group==5]
ratio6=ratio$zip[ratio$group==6]
ratio7=ratio$zip[ratio$group==7]

df1=zctas(cb=FALSE,starts_with = ratio1)
df2=zctas(cb=FALSE,starts_with = ratio2)
df3=zctas(cb=FALSE,starts_with = ratio3)
df4=zctas(cb=FALSE,starts_with = ratio4)
df5=zctas(cb=FALSE,starts_with = ratio5)
df6=zctas(cb=FALSE,starts_with = ratio6)
df7=zctas(cb=FALSE,starts_with = ratio7)

colors()[grep("sky",colors())]

leaflet(df) %>% addTiles() %>% addPolygons(fillOpacity = 0,color="blue",weight=2)%>%
  addPolygons(data=df1, color="cornflowerblue",fillOpacity = 0.7)%>%
  addPolygons(data=df2, color="skyblue",fillOpacity = 0.7)