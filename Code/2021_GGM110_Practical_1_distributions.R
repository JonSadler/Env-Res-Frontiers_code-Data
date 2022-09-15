#**********************************************************************************************************
# PRACTICAL CLASS 1 - Species distributions, ranges and population trends
# For the first part of this practical class we are going to the use the UK Butterfly Monitoring Scheme
# from the GBIF datastore
# URL: https://www.gbif.org/dataset/1e266c3d-92ef-4d5a-8e4a-c04742c772c3
# In your prelab you should have done some reading about this scheme
# The dataset is large and is all the active sites for the year 2015
#**********************************************************************************************************
# Updated: 27th Jan 2021 [updated for github Sept 2022]

# The packages below are all needed to run this practical. If you look at the bottom right window and click on the 'packages' 
# tab you can see if they are installed into your profile. 
# But we can use this code to automatically check the code and install packages if they don't exist
# <- <- <- <- <- <- <- <- 
if(!require(dplyr)) {
  install.packages("dplyr")}
if(!require(ggplot2)) {
  install.packages("ggplot2")}
if(!require(tidyr)) {
install.packages("tidyr")}
if(!require(ggmap)) {
  install.packages("ggmap")}
if(!require(gridExtra)) {
install.packages("gridExtra")}
# <- <- <- <- <- <- <- <-  

# Now were ae going to load them into the memory so they are available to you to use.
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(gridExtra)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOADING THE UKGMS DATA INTO R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I've preprocessed out all of the messy stuff and stored it as a CSV file (same format as the infiltration data you used with Veronica last semester)
# REMEMBER YOU HAVE TO RUN THE CODE FROM TOP TO BOTTOM!!!
# PLEASE export or screenshot the graphics and answer the questions as you get to them in the code
# This is not the assessment but you need to do it because the assessment will be a similar task relating to this
# week's practical or next week's one.


# READ IN THE DATAFILE
# It is called BMS2015.CSV
# We first need to select the location of the file you have downloaded. 
# You then need to get the code to read the file. 
# The next line opens a window - use it to find the data (it's like the windows explorer window#the CSV file you have selected and stores it in the 
# environment of R (the filename will appear on the top right hand window on the screen)

BMS  <- read.csv("add in the filename", header=T)

#See it's huge ~ 400K records (398444 rows) and 10 columns
dim(BMS)


#View it as a spreadsheet
View(BMS) # it's got 10 columns. Each row is a species record, at one site (lat-long), at a sample event (date)

# We need to organise the data slightly differently to look at species abundance patterns
# We'll use dplyr from the Tidyverse suite to do this. We could risk installing this: install.packages("tidyverse")
# But dplyr and ggplot2 are already in the CTL installation so we'll play it safe.
# We'll do the plotting with ggplot2. You were introduced to this library last semester

# We've already loaded the dplyr,tidyr and ggplot2 libraries 
# The commands we need are in the 'dplyr' library. We are going to group the records by the species and locations
# The piping operator in dplyr %>% allows us to run multiple commands together. Here we group_by and then summarise to count the records

BMS_rec <- BMS %>% group_by(family, genus, species, decimalLatitude, decimalLongitude) %>% summarise(Number_rec=sum(collectionCode)) 
# Now we have a new dataframe (called GMS_rec) with only 6 fields in it:
# family - family name of the butterfly
# generic name of the butterfly
# species - a column of species names (using the latin binomial not the common name)
# decimalLatitude - the decimal degree of latitude
# decimalLongitude - the decimal degree of longitude
# A new column (Number_rec) which is the number of records for each species at each site counted in 2015

#NOTE - we are making a number of assumptions here:
# 1. That the decimal degrees equate to a 1km square. They don't because bands of longitude vary in area.
# 2. That the decimal degrees are true spatial data. They aren't spatial objects; were keeping it
# straightforward because R will plot them in relation to each on a georeference base map (see Dr Kettridge's practical)
# Moreover, true spatial analyses in R is difficult to code without a bit more experience.
# 3. Lastly, we'll approximate abundances using the number of records (Number_rec). Not really correct but okay - it's a decent surrogate.
# Remember, within each BMS survey the number of individuals of each species was recorded but the GBIF data only indicate whether 
#it was present or not, rather than it's true abundance. See the collectionCode column in the datafile called BMS2015.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 1. MAPPING SPECIES DISTRIBUTIONS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [We can now plot maps of each species to examine their range patterns]. 

# To do this we need to load some packages (your are familiar with) and some new ones. 
# ggplot2 is a package that is used widely for plotting data witin R - and you've seen it before
# We will use ggplot with an associated function geom_point:

ggplot() + geom_point(data = BMS_rec, 
  aes(decimalLongitude,decimalLatitude,color=Number_rec),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Site abundance", color = "Number of records")

# A map of the UK - see lots more data in the south of the country. There is little variation in the numbers so most sites appear black
# We can then use dplyr's filter_by command to generate species maps. Here we select a common and rarer species.

# Common species - The green-veined white (Peris napi).
# Paste this link into your browser to see information on this species (or press alt click on the link):
# Check it out at https://butterfly-conservation.org/butterflies/green-veined-white

# We need the other columns (i.e. spatial location and number of records), so we select them; then filter on the species we're interested in.
Pieris_napi <- BMS_rec %>% select(family, genus, species, decimalLongitude,decimalLatitude, Number_rec) %>%
  filter(species == "Pieris napi")

# Basic (you've done this before)
ggplot() + geom_point(data = Pieris_napi, 
  aes(decimalLongitude,decimalLatitude,color=Number_rec),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Peris napi", color = "Number of records")

# Just like you did last semester we can beautify the map....using the ggmap package
# We've alreaded loaded it...!

# Then we want to define the boundary of our plot area. To do this we calculate the minimum and 
# maximum latitude and longitude

lat <- c(min(Pieris_napi$decimalLatitude),max(Pieris_napi$decimalLatitude))
long <- c(min(Pieris_napi$decimalLongitude),max(Pieris_napi$decimalLongitude))

# Define a box of interest
bbox <- make_bbox(long,lat,f=0.05)

# And then get a map of that area.
a <- get_map(bbox,maptype="toner-lite",source="stamen")
ggmap(a)

# If you want to find out more about how this mapping code works then check out the following
# Paste this link into your browser or press alt click on the link: https://medium.com/fastah-project/a-quick-start-to-maps-in-r-b9f221f44ff3

# Can then combine these two separate things together! To plot your data onto the map.

ggmap(a) + geom_point(data = Pieris_napi, 
  aes(decimalLongitude,decimalLatitude,color=Number_rec),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Green-veined White", color = "Number of Records")

# Try a rarer species such as the Swallow tail (Papilio machaon)
# Paste this link into your browser to see information on this species 
# (or press alt click on the link):https://butterfly-conservation.org/butterflies/swallowtail

Papilio_machaon <- BMS_rec %>% select(family, genus, species, decimalLongitude,decimalLatitude, Number_rec) %>%
  filter(species == "Papilio machaon")

# Draw the map....
ggmap(a) + geom_point(data = Papilio_machaon, 
  aes(decimalLongitude,decimalLatitude,color=Number_rec),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Swallow Tail", color = "Number of Records")

# To make comparisons easier you can plot both maps next to each other in one combined image (you can do more if you like)
# We use a gridExtra package  for this.....

# Pipe the plots into two objects called map1 and map2
map1 <- ggmap(a) + geom_point(data = Pieris_napi, 
  aes(decimalLongitude,decimalLatitude,color=Number_rec),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Green-veined White", color = "Number of Records")
map2 <- ggmap(a) + geom_point(data = Papilio_machaon, 
  aes(decimalLongitude,decimalLatitude,color=Number_rec),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Swallow Tail", color = "Number of Records")

#plot them side by side in two columns by one row
grid.arrange(map1, map2, nrow = 1)

# Click on the zoom magnifying glass icon to see it in a larger window!!!

# [Having read the ecological information on these species why do you think the green-veined white is 
# more widely distributed that the swallowtail?]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 2. EVALUATING SPECIES-ABUNDANCE and ABUNDANCE-RANGE PATTERNS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Recall also that we also examined the relationship between species abundances and ranges....

# Reformat the data using dplyr to count the number of records and the number of 1km squares
BMS_rn_abund <- BMS %>%     # Note we are starting with the first file we imported....
  group_by(species) %>%
  mutate(Number_rec=sum(collectionCode)) %>%
  mutate(Number_squ=length(unique(decimalLatitude)))  %>% 
  slice(1L)

# subset the data so we only have the columns we want [We'll analyse these data]
BMS_rn_abund <- subset(BMS_rn_abund, select = c("species", "Number_rec", "Number_squ"))

# Now let's test our two hypothesess by plotting a histogram of the number of records (our proxy for abundance)
# TEST 1: Species Abundance relationships
# This is the same code that you used in the catchment hydrology practical

hist(BMS_rn_abund$Number_rec, main="Species-Abundance plot", 
     breaks = 20,
     xlab="Abundance", 
     border="white", 
     col="black",
     xlim = c(0, 60000)) # This just adds limits to the extent of the x axis

#[What does this tell you about the species - abundance pattern? Does it fit our hypothesis?]
# Note we have a big outlier - a 'superabundant' species (~55,000 records) which is the Meadow Brown (Maniola jurtina)

# And if we base2 log the axis....?
BMS_rn_abund$Log_Number_rec <- log2(BMS_rn_abund$Number_rec) 

# replot the figure....
hist(BMS_rn_abund$Log_Number_rec, main="Species-Abundance plot", 
     breaks = 20,
     xlab="Abundance", 
     border="white", 
     col="black",
     xlim = c(0,20))
# hmmmm - not so hump-shaped. It seems we have a larger number of common species
# than we might have expected. See the skew on the right of the plot.

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Now lets test the species abundance - range prediction by plotting the number of records (our proxy for abundance)
# against the number of BMS sites (~1km squares) (our proxy for range) that the butterflies were recorded in during 2015.
# TEST 2: Abundance - range patterns

# first a straight linear line....doesn't fit too well
ggplot(BMS_rn_abund, aes(x=Number_squ, Number_rec)) + # This code tells ggplot the data (BMS_rn_abund); what X and Y are
  geom_point() +  # this tells R to add the data points (there are 61 = each dot is one species)
  geom_smooth(method=lm)+ # This tells R to draw a straight (trend) line through the data
  labs(title="Range-abundance of UK butterfly species", # This line adds a title and axis labels
  x="Distribution/range", y = "Abundance")

# Second, a curved line (fits the data better)
ggplot(BMS_rn_abund, aes(x=Number_squ, Number_rec)) + 
  geom_point()+
  geom_smooth(method=lm, formula=y ~ poly(x, 3, raw=TRUE)) + # difference here is the formula that tells R to fit a curve to the data
  labs(title="Range-abundance of UK butterfly species",
       x="Distribution/range", y = "Abundance")

# [What does this tell us about our prediction / hypothesis....?]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 3. EVALUATING TEMPORAL PATTERNS AND TRENDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For this you'll need the second datafile....
# butterflies_uk_temporal_data.CSV
# You then need to get the code to read the file. 
# The next line opens a search window; use it to find the CSV file and select it 
# It will store it in the environment of R (the filename will appear on the top right hand window on the screen)

BMS_time  <- read.csv("add in datafile name", header=T)

# There have been some issues with importing this file with the column names so we'll rename them

names(BMS_time) <- c("Year", "Generalists", "Specialists")
# look at it...
View(BMS_time)

# Most of these plot functions you've encountered. The key element here is the type = "b" argument
# This plot the lines between the points; check help ?plot for other formats options

plot(BMS_time$Specialists,type="b",lwd=2,
     xaxt="n",ylim=c(0,100),col="red", # xaxt surpresses the X axis plotting because we're going to control it using axis()
     xlab="Year",ylab="Population Size",
     main="Temporal changes in British Butterflies") # We'll add a title to this one too
axis(1,at=1:length(BMS_time$Year),labels=BMS_time$Year) # sets the X axis to years using appropriate labels
lines(BMS_time$Generalists,col="blue",type="b",lwd=2) # Add additional lines with different colour for the generalists

# Add a legend
legend("topright",legend=c("Generalist","Specialist"), # Adds legend with labels to top right of plot
       lty=1,lwd=2,pch=21,col=c("blue","red"), # sets line type, width, dot type/size and colour to the same as the plot
       ncol=2,bty="n",cex=0.8,
       text.col=c("blue","red"), # Sets the text to the same colour as the lines
       inset=0.01) # Sets the inset distance from the margins as a factor of plot size

#[What are the key three patterns visible in the plot?]

