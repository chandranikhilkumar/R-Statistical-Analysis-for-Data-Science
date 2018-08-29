library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)

# Get a shape file of states in the US

usa.df <- map_data("state")
print(usa.df)
colnames(usa.df)[5] <- "State"

# Get the data to be plotted

usa.dat <- read.table("C:/Users/nxc161330/Documents/R/us_2016_election_data.csv", header = T, sep = ",")
usa.dat$State <- tolower(usa.dat$State)
colnames(usa.dat)[2] <- "Clinton"
colnames(usa.dat)[3] <- "Trump"
colnames(usa.dat)[4] <- "Others"
usa.dat$Clinton <- as.numeric(sub("%", "", usa.dat$Clinton, fixed=TRUE))/100
usa.dat$Trump <- as.numeric(sub("%", "", usa.dat$Trump, fixed=TRUE))/100
usa.dat$Others <- as.numeric(sub("%", "", usa.dat$Others, fixed=TRUE))/100



usa.dat1 <- usa.dat[usa.dat$Trump > usa.dat$Clinton, c("State", "Trump")]
usa.dat2 <- usa.dat[usa.dat$Trump <= usa.dat$Clinton, c("State", "Clinton")]

print(usa.dat1)
print(usa.dat2)
# Merge the data with the shape file

usa.df1 <- join(usa.df, usa.dat1, by = "State", type = "inner")
usa.df2 <- join(usa.df, usa.dat2, by = "State", type = "inner")

# Abbreviations of states and where thy should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) #%in% usa.df$State # exclude Hawaii as there is no data for this state
states <- states[subset, ]

# Write a function that does the plotting (Feel free to play around 
# with the options)
brks.to.use <- seq(0, 1, by = 0.1) # give intervals:  
# 10-14 (less than 14), 14-18, ..., 30-34 (>= 30)
figure.title <- "Presidential Election Results in 2016"

p1 <- ggplot() + 
# Draw borders of states
geom_polygon(data = usa.df1, aes(x = long, y = lat, group = group, 
fill = Trump), color = "black", size = 0.15) + 
# Use shades of red for plotting; trans = "reverse" option 
# makes the shades go from dark to light as the income share increases, 
# ensuring that darkest red = worst case scenario.
scale_fill_distiller(palette = "Reds", breaks = brks.to.use,
trans = "reverse") + 
 
# Add legend
theme_nothing(legend = TRUE) + labs(title = figure.title, fill = "") + 
# Add state abbreviations
geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)

p2 <- ggplot() +
 
  # Draw borders of states
  geom_polygon(data = usa.df2, aes(x = long, y = lat, group = group, 
                                   fill = Clinton), color = "black", size = 0.15) + 
  # Use shades of red for plotting; trans = "reverse" option 
  # makes the shades go from dark to light as the income share increases, 
  # ensuring that darkest red = worst case scenario.
  scale_fill_distiller(palette = "Blues", breaks = brks.to.use,
                       trans = "reverse") + 
  
  # Add legend
  theme_nothing(legend = TRUE) + labs(title = figure.title, fill = "") + 
  # Add state abbreviations
  geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
 


ggsave(p1, height = 4, width = 4*1.9,
file = "C:/Users/nxc161330/Documents/R/2016_US_Election_Results_Trump.jpg")

#ggsave(p2, height = 4, width = 4*1.9,
      # file = "C:/Users/nxc161330/Documents/R/2016_US_Election_Results_Clinton.jpg")