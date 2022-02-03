# Activate essential packages
library(plyr) # this package needs to be loaded before dplyr to reduce potential conflicts
library(tidyverse)
library(viridis)
library(data.table)
library(readr)
library(ggplot2)
library(sf)
library(tinytex)
library(classInt)
library(sf)

# Create a collection of .csv files saved into the current directory, first by defining the working directory where the csv files are located.
mydir = "~/Documents/psych_hospital/pph_csv_files"

# Then specify the filepath and filetypes to search for, and include the full file names in the object
myfiles = list.files(path = mydir, pattern = "*.csv", full.names = TRUE)

# Inspect the object - there should be 32 files
myfiles

# Join the files using ldply from the plyr package
all_data = ldply(myfiles, read_csv)

# View the first 10 rows of data within the RMarkdown window below
all_data

# Create a tibble of the files by combining all files into one using column headers to join the data
all_data <- as_tibble(all_data)

# Save the tibble as a data file readable by R
save(all_data, file = "all_data.rda")

# Make a backup file from the amalgamated .csv files as one .csv
write_excel_csv(all_data, "all_data.csv")

class(all_data) # Check the type of object to make sure data are in data.frame format
names(all_data) # Look at the column names to make sure there are no errors and identify if any names need to be changed for clarity
dim(all_data) # Dimensions of the dataset - number of rows and columns
str(all_data) # Check the data structure to see if variables are categorised correctly
summary(all_data) # Generate summary statistics to check for e.g. potential anomalies, missing values, minimum and maximum values, relationship between median and mean

# Format the text in the period column to show just the year ranges without the additional text:
all_data$period <- gsub(" financial years; 3-year aggregates", "", all_data$period)

# Then replace the forward slash (/) with a hyphen (-)
all_data$period <- gsub ("/", "-", all_data$period)

# Subset data to create a smaller dataframe of all rows, and columns 2 to 8 inclusive.
all_data_subset <- all_data[, 2:8]

# Change 'period' to factor to ensure it renders correctly on graphs and charts
all_data_subset$period <- factor(all_data_subset$period)

# Change quintile to factor
all_data_subset$quintile <- factor(all_data_subset$quintile)

# Check for unique and missing values
all_data_subset$numerator %>% unique()
all_data_subset$numerator %>% table()

# Boxplot

# Call ggplot and create a subset of data from the quintiles
# Specify the levels within the quintile column with quintile %in% c("1 - most deprived", "5 - least deprived")
# Call the aesthetics to plot time period on the x axis and indicator_measure on the y axis. Boxplots are colour-coded by quintile (using the viridis palette).
# geom_boxplot(width = 0.5) alters the width of each boxplot to improve fit on the page
# Labels are added with 'labs', where title, subtitle and caption can be specified
# The viridis palette is used to fill the boxplots
# The two groups of boxplots are placed side by side using facet_wrap based on quintile order
# Additional aesthetics are managed by the theme_bw and theme() functions.
# Theme_bw creates a simple theme with limited formatting; it is important to use theme() for adjusting other elements of formatting as it has greater flexibility than theme_bw

ggplot(subset(all_data_subset, 
              quintile %in% c("1 - most deprived", "5 - least deprived")), 
       aes(x = period, y = indicator_measure, fill = quintile)) + 
  geom_boxplot(width = 0.5) +
  labs(title = "Comparison of Psychiatric Patient Hospitalisations from most deprived\nand least deprived areas of Scotland, 2002 to 2020",
       subtitle = "Age-standardised rate per 100,000",
       caption = "Source: Scottish Public Health Observatory 2021") +
  scale_fill_viridis_d(alpha = 0.5) +
  facet_wrap(quintile~.) +
  theme_bw() +
  theme(panel.background = element_blank(), # Removes heavy background formatting
        panel.border = element_blank(), # Removes the outline border around the plot
        strip.background = element_rect(fill = "white"), # Changes the background fill from the plot titles ("1 most deprived", "5 - least deprived") from gray to colour of choice
        strip.text.x = element_text(size = 12), # Changes text size of plot titles ("1 most deprived", "5 - least deprived")
        legend.position = "none", # Removes the legend
        plot.title = element_text(vjust = 1, lineheight = 1.15), # Moves the vertical alignment of the plot title and increases the spacing between the title lines
        plot.subtitle = element_text(size = 11, vjust = 1, lineheight = 1.5), # Modifies the features of the subtitle: text size, vertical adjustment and lineheight
        plot.caption = element_text(size = 10, vjust = -1, hjust = 0), # Modifies the caption attributes, including horizontal adjustment
        axis.title.x = element_blank(), # Removes the x axis title
        axis.title.y = element_blank(), # Removes the y axis title
        axis.text.x = element_text(size = 10, color = "black", angle = 90), # Modifies the x axis text
        axis.text.y = element_text(size = 10, color = "black")) # Modifies the y axis text

# One approach is to filter the appropriate factor levels from 'quintile'

quint_diff <- all_data_subset %>%
  filter(quintile %in% c("1 - most deprived", "5 - least deprived"))

# View the first few rows to check all is ok
head(quint_diff)

# Then reshape the data and carry out the calculation before returning the data to 'long', or tidy format:

deprivation_diff <- pivot_wider(data = quint_diff, # Create a new variable, using the filtered data from the previous step
                                id_cols = c(geography_code, period), # Specify the columns to be reshaped
                                names_from = quintile, # Select the names of the factor levels to be new columns
                                values_from = indicator_measure) %>% # Apply the appropriate values to their quintiles
  mutate(diff = `1 - most deprived` - `5 - least deprived`) %>% # Create a new column called 'diff', subtracting the indicator_measure of quintile 5 from quintile 1
  pivot_longer(cols = c('1 - most deprived', '5 - least deprived'), # Reshape the data into tidy format by returning the quintiles to one column
               names_to = "quintile", # Restore the original name
               values_to = "indicator_measure") # Restore values to their respective levels in the quintile column

# View the first few rows to data to make sure it works
head(deprivation_diff)

# Histograms

# Call ggplot
# Use the newly calculated differences between quintiles 1 and 5 for the x axis
# geom_histogram calls the histogram function, with bins set to 20 and colour aesthetics modified to suit
# Labels are added with 'labs', where title, caption and x and y axes specified
# The groups of histograms are placed in four rows of four using facet_wrap based on time period
# Additional aesthetics are managed by the theme_bw and theme() functions as in the previous boxplots, but with some amendments based on additional text elements

ggplot(data = deprivation_diff, aes(x=diff)) +
  geom_histogram(col = "white", fill = "blue", alpha = 0.5, bins = 15) +
  labs(title = "Psychiatric Patient Hospitalisations histograms showing differences between most deprived and least deprived areas of Scotland\n2002 to 2020",
       caption = "Source: Scottish Public Health Observatory 2021",
       x = "Age-standardised rate per 100,000",
       y = "Count") +
  facet_wrap(period~.) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(vjust = 1, lineheight = 1.15),
        plot.subtitle = element_text(size = 11, vjust = 1, lineheight = 1.5),
        plot.caption = element_text(size = 10, vjust = -1, hjust = 0),
        axis.title.x = element_text(size = 11, vjust = -0.5),
        axis.title.y = element_text(size = 11, vjust = 1),
        axis.text.x = element_text(size = 11, color = "black"),
        axis.text.y = element_text(size = 11, color = "black"))

# Read the shapefile into R with the following code (this may not work in RMarkdown due to errors with the shapefile, but it executes from the console)

scotland_map <- read_sf(dsn = "~/Documents/Local_Authority_Boundaries_-_Scotland", "pub_las")

# Check the type of geometry - such as polygons, multipolygons
st_geometry_type(scotland_map)

# In some cases it may be necessary to alter the projects, particularly when working with many shapefiles and datasets. To do this it is essential to know the projection. Here it is OSGB 1936 / British National Grid
st_crs(scotland_map)

# Joing the shapefile to the hospitalisation data
# Create a new object and merge the data on column 'code' from the scotland_map shapefile just created and 'geography code' from the deprivation_diff dataset
map_data_depdiff <- merge(scotland_map, deprivation_diff, by.x = "code", by.y = "geography_code")

# Select the most recent set of data
map_2020 <- map_data_depdiff %>%
  filter(period == "2017-18 to 2019-20")

# Create breaks in the data using classIntervals function, and specifying the diff variable and rounding, number of breaks, and the method by which this information is to be interpreted by ggplot
# Nine breaks are to be used
breaks_qt_2020 <- classIntervals(c(min(map_2020$diff) - .0001, map_2020$diff), n = 9, style = "quantile")

# Add these breaks to the mapping object combining the shapefile and hospitalisation dataset
quantile_breaks_2020 <- mutate(map_2020, diff_cat = cut(diff, breaks_qt_2020$brks))

# View the data
quantile_breaks_2020

# Plot the data onto a map using the object just created
# Use geom_sf to plot the shapefile and use the breaks specified in previous steps to categorise the council areas and use the correct colour based on the differences between quintiles 1 and 5
# A palette of blues, useful for colour-blindness, is used to colour the areas of the map according to the breaks
# Legend labels have been changed from the breaks values to something easier to interpret
# Similar theme formatting has been applied

p1 <- ggplot(quantile_breaks_2020) +
  geom_sf(aes(fill = diff_cat)) +
  labs(title = "Comparison of Psychiatric Patient Hospitalisations in Scotland attributable to deprivation\n2017-18 to 2019-2020",
       caption = "Source: Scottish Public Health Observatory 2021",
       fill = "Deprivation categories") +
  scale_fill_brewer(palette = "Blues",
                    labels = c("1 - least deprivation", "2", "3", "4", "5", "6", "7", "8", "9 - most deprivation")) +
  theme_bw() +
  theme(panel.background = element_blank(), # Removes background colour
        panel.border = element_blank(), # Removes the borders
        panel.grid.major = element_blank(), # Removes the gridlines
        plot.title = element_text(vjust = 1, lineheight = 1.15),
        plot.subtitle = element_text(size = 11, vjust = 1, lineheight = 1.5),
        plot.caption = element_text(size = 10, vjust = -1, hjust = 0),
        axis.text.x = element_blank(), # Removes the latitude/ longitude
        axis.text.y = element_blank(), # Removes the latitude/ longitude
        axis.ticks = element_blank()) # Removes all latitude/ longitude markers

# View the map
p1