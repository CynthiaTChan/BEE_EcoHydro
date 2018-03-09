# Ecohydrology 2016
# Week 3
# What portion of the precipitation is lost as evapotranspiration in a watershed? 

# Load "EcoHydRology" Package
library(EcoHydRology)

# Step 1: Set your working directory to the folder where you've stored your precipitation data and load that data into the R environment.
setwd("~/Google Drive/Cornell/Ecohydrology/Week3")

# Step 2: Download USGS streamflow data using the EcoHydRology package
# Fall Creek, Ithaca, NY: "04234000"
FC <- get_usgs_gage(flowgage_id = "04234000", begin_date = "1950-01-01", end_date="2016-12-31")

# Step 3: Find the size of the watershed for a USGS Streamgage, Convert streamflow to mm / day
# Tip: get_usgs_gage function returns area in square kilometers.
FC_area <- FC$area  # Area of the watershed in square kilometers
FC_flow <- FC$flowdata$flow   # Flow in cubic meters per day
FC_flow_depth <- FC_flow / FC_area / 1000 # Flow depth in mm/day

#Step 4: calculate the annual total precipitation and streamflow
MetData <- read.csv("GameFarmRd_1950-present.csv")

#Convert Precip and Temp to SI units, from (in -> cm) and (F -> C)
#Note, this is not a vector operation
for (i in 1:nrow(MetData))
{
  MetData$Precip_mm[i] = MetData$Precip[i]*25.4
  MetData$Tmax_C[i] = 5/9*(MetData$Tmax[i]-32)
  MetData$Tmin_C[i] = 5/9*(MetData$Tmin[i]-32)
}  

# Generate the annual precipitation and stream flow from the daily values

FC$date <- FC$flowdata$date
FC$flowdata$Year = as.numeric(format(as.Date(FC$date),"%Y"))
FCmatrix = data.frame(matrix(nrow = length (FC$flowdata$date), ncol = 0))
FCmatrix$Year = FC$flowdata$Year
FCmatrix$Flowdepth = FC_flow_depth
AnnualData <- data.frame(matrix(nrow = 2017-1950, ncol=0))

  
for (i in 1950:2016)
{
  AnnualData$Year[i-1949] = i # years
  AnnualData$Precip_mm[i-1949] = sum(MetData[MetData$Year == i,7]) # precip in mm
  AnnualData$Stream_mmday[i-1949] = sum(FCmatrix[FCmatrix$Year == i,2 ]) # stream depth in mm/day
}  

# Step 5: Calculate the annual evapotranspiration for each year
# Hint: Assume water storage in a watershed is neglible over a year.
# storage = Precip - ET - streamflow
# Assume storage = 0
# ET = Precip - streamflow 

AnnualData$ET = AnnualData$Precip_mm - AnnualData$Stream_mmday
AnnualData$ETratio = AnnualData$ET/AnnualData$Precip_mm


# Step 6: Calculate and plot the annual portion of precipitation that is evapotranspiration 
#Plot the trends of Precip, ET, and ET Ratio


#Step 7: Fit a linear model to the data with x=year and y=ET
#Is there a significant trend?
#hint: ?lm
plot.new()
plot(AnnualData$Year, AnnualData$ET, xlim = c(1950,2017), ylim = c(200,600), 
     xlab = "Year", ylab = "mm", main = "ET vs time in Fall Creek" ) 
abline(lm(AnnualData$ET ~ AnnualData$Year))
summary(lm(AnnualData$ET ~ AnnualData$Year))
pvalue_ET <- summary(lm(AnnualData$ET ~ AnnualData$Year))$coefficients[2,4]
# The p-value is 0.1844, this is not statistically significant given our threshold of 0.1. 

#Step 8: Fit a linear model to the data with x=year and y=P
#Is there a significant trend?
#hint: ?lm
plot.new()
plot(AnnualData$Year, AnnualData$Precip_mm, xlim = c(1950,2017), ylim = c(500,1300), 
     xlab = "Year", ylab = "mm", main = "Precipitation vs time in Fall Creek" ) 
abline(lm(AnnualData$Precip_mm ~ AnnualData$Year))
summary(lm(AnnualData$Precip_mm ~ AnnualData$Year))
pvalue_precip <- summary(lm(AnnualData$Precip_mm ~ AnnualData$Year))$coefficients[2,4]
# The p-value is 0.1562, this is not statistically significant given our threshold of 0.1. 

#Step 8.5: Fit a linear model to the data with x=year and y=ET/P
#Is there a significant trend?
#hint: ?lm
plot.new()
plot(AnnualData$Year, AnnualData$ETratio, xlim = c(1950,2017), ylim = c(0,1), 
     xlab = "Year", ylab = "mm", main = "ET/P vs time in Fall Creek" ) 
abline(lm(AnnualData$ETratio ~ AnnualData$Year))
summary(lm(AnnualData$ETratio ~ AnnualData$Year))
pvalue_ETratio <- summary(lm(AnnualData$ETratio ~ AnnualData$Year))$coefficients[2,4]
# The p-value is 0.9868, this is not statistically significant given our threshold 0.1.

##############################################################################################

#Step 9: Choose a new watershed in a different climate region, collect streamflow and precip data and repeat this analysis.
# Albuquerque, NM;USGS gage 08330000, a different climate region! 
# https://waterdata.usgs.gov/nwis/uv?site_no=08330000


# Step 2: Download USGS streamflow data using the EcoHydRology package

NM <- get_usgs_gage(flowgage_id = "08330000", begin_date = "1950-01-01", end_date="2016-12-31")

# Create an empty matrix to add daily flows and do year conversions
NM_daily_flow <- data.frame(matrix(nrow = length(NM$flowdata$flow), ncol = 0))
NM_daily_flow$Date <- NM$flowdata$date
NM_daily_flow$Year = as.numeric(format(as.Date(NM_daily_flow$Date),"%Y"))

# Step 3: Find the size of the watershed for a USGS Streamgage, Convert streamflow to mm / day
# Tip: get_usgs_gage function returns area in square kilometers.
NM_area <- NM$area  # Area of the watershed in square kilometers
NM_flow <- NM$flowdata$flow   # Flow in cubic meters per day
NM_daily_flow$Flow_depth_mmday <- NM_flow / NM_area / 1000 # Flow depth in mm/day


#Step 4: calculate the annual total precipitation and streamflow
AnnualAlbPRCP <- read.csv("AlbPrcp.csv")

#Convert Precip and Temp to SI units, from (in -> cm) and (F -> C)
#Note, this is not a vector operation
for (i in 1:nrow(AnnualAlbPRCP))
{
  AnnualAlbPRCP$Precip_mm[i] = AnnualAlbPRCP$PRCP[i]*25.4
}  

# Get all annual data! Let's put the flow into the AnnualAlbPRCP matrix

for (i in 1950:2016)
{
  AnnualAlbPRCP$Stream_mmyear[i-1949] = sum(NM_daily_flow[NM_daily_flow$Year == i,3 ]) # stream depth in mm/year
}  

# Step 5: Calculate the annual evapotranspiration for each year
# Hint: Assume water storage in a watershed is neglible over a year.
# storage = Precip - ET - streamflow
# Assume storage = 0
# ET = Precip - streamflow 

AnnualAlbPRCP$ET = AnnualAlbPRCP$Precip_mm - AnnualAlbPRCP$Stream_mmyear
AnnualAlbPRCP$ETratio = AnnualAlbPRCP$ET/AnnualAlbPRCP$Precip_mm

#Step 7: Fit a linear model to the data with x=year and y=ET
#Is there a significant trend?
#hint: ?lm
plot.new()
plot(AnnualAlbPRCP$DATE, AnnualAlbPRCP$ET, xlim = c(1950,2017), ylim = c(100,325), 
     xlab = "Year", ylab = "mm", main = "ET vs time in Albuquerque" ) 
abline(lm(AnnualAlbPRCP$ET ~ AnnualAlbPRCP$DATE))
summary(lm(AnnualAlbPRCP$ET ~ AnnualAlbPRCP$DATE))
NM_pvalue_ET <- summary(lm(AnnualAlbPRCP$ET ~ AnnualAlbPRCP$DATE))$coefficients[2,4]
# The p-value is 0.04211, this is statistically significant given our threshold of 0.1. 

#Step 8: Fit a linear model to the data with x=year and y=P
#Is there a significant trend?
#hint: ?lm
plot.new()
plot(AnnualAlbPRCP$DATE, AnnualAlbPRCP$Precip_mm, xlim = c(1950,2017), ylim = c(100,350), 
     xlab = "Year", ylab = "mm", main = "Precip vs time in Albuquerque" ) 
abline(lm(AnnualAlbPRCP$Precip_mm ~ AnnualAlbPRCP$DATE))
summary(lm(AnnualAlbPRCP$Precip_mm ~ AnnualAlbPRCP$DATE))
NM_pvalue_Precip <- summary(lm(AnnualAlbPRCP$Precip_mm ~ AnnualAlbPRCP$DATE))$coefficients[2,4]
# The p-value is 0.04772, this is statistically significant given our threshold of 0.1. 

#Step 8.5: Fit a linear model to the data with x=year and y=ET/P
#Is there a significant trend?
#hint: ?lm
plot.new()
plot(AnnualAlbPRCP$DATE, AnnualAlbPRCP$ETratio, xlim = c(1950,2017), ylim = c(0.6,1), 
     xlab = "Year", ylab = "" ,main = "ET ratio vs time in Albuquerque" ) 
abline(lm(AnnualAlbPRCP$ETratio ~ AnnualAlbPRCP$DATE))
summary(lm(AnnualAlbPRCP$ETratio ~ AnnualAlbPRCP$DATE))
NM_pvalue_ET <- summary(lm(AnnualAlbPRCP$ETratio ~ AnnualAlbPRCP$DATE))$coefficients[2,4]
# The p-value is 0.9262, this is not statistically significant given our threshold 0.1.


