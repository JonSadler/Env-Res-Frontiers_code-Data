#**********************************************************************************************************
# BIOGEOGRAPHY - PRACTICAL CLASS 2 - Species Gradients
# In your prelab you should have done some reading about the OPAL Air Quality Survey
# And undertaken a survey of lichen species on two trees in a local open space
# One tree (Tree 1) will be close to major road
# The other (Tree 2) should have been located over 100m from the road
# updated Feb 4th 2022
# Author: Jon Sadler
#**********************************************************************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOADING THE UKGMS DATA INTO R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We'll loading in the excel spreadsheet you created with you surveys
# REMEMBER YOU HAVE TO RUN THE CODE FROM TOP TO BOTTOM!!!
# PLEASE export or screenshot the graphics and answer the questions as you get to them in the code
# This is not the assessment but you need to do it because the assessment will be a similar task relating to this
# week's practical or next week's one.

# PLEASE MAKE SURE THAT YO SET YOUR WORKING DIRECTORY CORRECTLY
# THERE IS GUIDANCE HERE CANVAS SITE ON HOW TO DO THAT.

# READ IN THE DATAFILE
# It is called lichen_dataset.csv

# You then need to get the code to read the file. The next line opens the CSV file you have selected and stores it in the 
# Environment of R (the filename will appear on the top right hand window on the screen)

Lichen  <- read.csv("lichen_dataset.csv",header=TRUE)

#View it as a spreadsheet
View(Lichen) 
# it's got 14 columns (including 9 species of lichen). 
# Each row is a species record (abundance code = 0-3) of lichens on a tree, at one site (lat-long), at a sample event (date)

# We need to organise the data slightly differently to look at species abundance patterns
# As in practical 1, we'll use dplyr from the Tidyverse suite to do this. We could risk installing this: install.packages("tidyverse")
# But dplyr and ggplot2 are already in the CTL installation so we'll play it safe.
# We'll do the plotting with ggplot2. Nick introduced you to this library last semester

# Load the dplyr,tidyr and ggplot2 libraries - ignore the warning messages about masking and version control


	library("ggplot2")
	library("tidyverse")

# The commands we need are in the 'dplyr' library. 
# We are going to use the mutate function in 'dplyr' to calculate the means of lichens within each group
# This will a create new datafile with three new variables (N_sensitive, N_loving, N_intermediate)

# The piping operator in dplyr %>% allows us to run multiple commands together. 
# Here we use the first datafile, and tell it to calculate a mean for each group of lichens (by rows)

Lichen_groups <- Lichen %>% rowwise() %>% mutate(N_sensitive = mean(Usnea,Evernia,Hypogymnia),
        N_intermediate = mean(Melanelixia,Flavoparmelia,Parmelia), 
        N_loving = mean(Leafy_Xanthoria,Cushion_Xanthoria,Physcia)) %>%
          select(Latitude,Longitude,Tree_no,Tree_sp,N_sensitive,N_intermediate, N_loving)

# Now we have a new dataframe (Lichen_groups) with only 7 columns in it:
# Latitude - the decimal degree of latitude
# Longitude - the decimal degree of longitude
# Tree_no - Tree 1 or Tree 2
# Tree species - the common name of the species of tree with the lichen on it
# A new column (N_sensitive) which is the mean of the  3 N-sensitive lichen species
# A new column (N_intermediate) which is the mean of the 3 N-intermediate lichen species
# A new column (N_loving) which is the mean of the 3 N-loving lichen species

# Have a look at it.....
View(Lichen_groups)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 1. MAPPING SPECIES DISTRIBUTIONS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [We can now plot maps of each gropu to examine their distributions]. 

# To do this we need to load some packages (your are familiar with) and some new ones. 
# ggplot2 is a package that is used widely for plotting data witin R - and you've seen it before
# We will use ggplot with an associated function geom_point:

# First we'll plot the N-sensitive species
ggplot() + geom_point(data = Lichen_groups, 
  aes(Longitude,Latitude,color=N_sensitive),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="N-Sensitive species abundance", color = "Mean abundance")

# Now we'll plot the N-intermediate species
ggplot() + geom_point(data = Lichen_groups, 
                      aes(Longitude,Latitude,color=N_intermediate),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="N-Intermediate species abundance", color = "Mean abundance")

# Now we'll plot the N-loving species
ggplot() + geom_point(data = Lichen_groups, 
                      aes(Longitude,Latitude,color=N_loving),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="N-Loving species abundance", color = "Mean abundance")

# Just like you did with Dr Kettridge (Nick) we can beautify the map....
# Again we can install a package into the R script to help us do this. We are going to use ggmap again.

# install.packages("ggmap"). We don't need to do that as it is there already
library(ggmap)

# Then we want to define the boundary of our plot area. To do this we create a box around the city
# using latitude and longitude values I've estimated from google earth

lat <- c(52.4380,52.50)
long <- c(-1.9356,-1.87)

# Define a box of interest
bbox <- make_bbox(long,lat,f=0.05)

# And then get a map of that area.
a <- get_map(bbox,maptype="toner-lite",source="stamen")
ggmap(a)

# If you want to find out more about how this mapping code works then check out the following
# Paste this link into your browser or press alt click on the link: https://medium.com/fastah-project/a-quick-start-to-maps-in-r-b9f221f44ff3

# Can then combine these two separate things together! To plot your data onto the map.

ggmap(a) + geom_point(data = Lichen_groups, 
  aes(Longitude,Latitude,color=N_sensitive),size=3,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="N-Sensitive Species", color = "Mean Abundance")

#----------------------------------------------
# CLASS TASK - repeat the above code for N_intermediate and N_loving.

# To do this copy the block of code above and paste it below.
# Change the aes colour= function to the appropriate column (either N_intermediate or N_loving)

# Don't forget to change the title= function too....
#-------------------------------------------------
# ADD N_intermediate CODE IN HERE




#-------------------------------------------------
# ADD N_loving CODE IN HERE




#-------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 2. Visualising the gradient for nitrogen-sensitive species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We'll use a barchart to see if there are different mean abundance levels on the two trees
# But first we need to calculate the means of each lichen group by trees (numbers 1 and 2)
# We'll start with Nitrogen sensitive species
# We can get R to create this small datatable using this following code:
N_sensitive_chart <- tapply(Lichen_groups$N_sensitive, INDEX = Lichen_groups$Tree_no, FUN = mean)

# Simple Bar Plot 
barplot(N_sensitive_chart, main="Nitrogen sensitive species of lichens", 
        xlab="Tree Number", ylab = "Abundance")

# Add in some colour
barplot(N_sensitive_chart, main="Nitrogen sensitive species of lichens", 
        xlab="Tree Number", ylab = "Abundance", col = "powderblue")

#-------------------------------
# QUESTION
# What does the pattern suggest?
# ANNOTATE YOUR ANSWERS HERE
#
#
#
#
#
#
#
#---------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 3. Visualising the gradient for nitrogen-intermediate species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now nitrogen intermediate species
# Only difference in the code is that we are swapping 'N_sensitive' with 'N_intermediate'
# And changing the title

N_intermediate_chart <- tapply(Lichen_groups$N_intermediate, INDEX = Lichen_groups$Tree_no, FUN = mean)

barplot(N_intermediate_chart, main="Nitrogen intermediate species of lichens", 
        xlab="Tree Number", ylab = "Mean Abundance", col = "powderblue")

#-------------------------------
# QUESTION
# What does the pattern suggest?
# ANNOTATE YOUR ANSWERS HERE
#
#
#
#
#
#
#
#---------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 4. Visualising the gradient for nitrogen-loving species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now nitrogen loving species
# Only difference in the code is that we are swapping 'N_intermediate' with 'N_loving'
# And changing the title
N_loving_chart <- tapply(Lichen_groups$N_loving, INDEX = Lichen_groups$Tree_no, FUN = mean)

barplot(N_loving_chart, main="Nitrogen loving species of lichens", 
        xlab="Tree Number", ylab = "Mean Abundance", col = "powderblue")

#-------------------------------
# QUESTION
# What does the pattern suggest?
# ANNOTATE YOUR ANSWERS HERE
#
#
#
#
#
#
#
#---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 5. We can also look at whether the lichen species are distributed differently on different tree species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We can also use a barchart to see if there are different abundance levels the different tree species
# The only difference in the code is that we replaced 'Tree_no' with 'Tree_sp' when calculating the mean

# Nitrogen sensitive
N_sensitive_chart_tree_sp <- tapply(Lichen_groups$N_sensitive, INDEX = Lichen_groups$Tree_sp, FUN = mean)
barplot(N_sensitive_chart_tree_sp, main="Nitrogen sensitive species of lichens", 
        xlab="Tree Number", ylab = "Mean Abundance", col = "powderblue")

# Nitrogen intermediate
N_intermediate_chart_tree_sp <- tapply(Lichen_groups$N_intermediate, INDEX = Lichen_groups$Tree_sp, FUN = mean)
barplot(N_intermediate_chart_tree_sp, main="Nitrogen intermediate species of lichens", 
        xlab="Tree Number", ylab = "Mean Abundance", col = "powderblue")

# Nitrogen loving
N_loving_chart_tree_sp <- tapply(Lichen_groups$N_loving, INDEX = Lichen_groups$Tree_sp, FUN = mean)
barplot(N_loving_chart_tree_sp, main="Nitrogen loving species of lichens", 
        xlab="Tree Number", ylab = "Mean Abundance", col = "powderblue")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TASK 6. Testing the significance of the gradient patterns (i.e. comparing trees 1 and 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To test whether the patterns are statistically different we again use some basic statistics
# Run a Wilcoxon test to see if groups of species differ in relation to the trees and nearness to the road (i.e. pollution source)

wilcox.test(N_sensitive ~ Tree_no, data = Lichen_groups) 

# We repeat for other groups of lichens
wilcox.test(N_intermediate ~ Tree_no, data = Lichen_groups) 
wilcox.test(N_loving ~ Tree_no, data = Lichen_groups) 

#-------------------------------
# QUESTION
# So what are these tests telling us about lichen abundance on this pollution gradient?
# ANNOTATE YOUR ANSWERS HERE
#
#
#
#
#
#
#
#---------------------------------
# Remember we are looking for P values of less than 0.05 for a statistically significant pattern
