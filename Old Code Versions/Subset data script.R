
# Script for filtering and subsetting BEF snow depth photos from NEON

library(exifr)
library(tidyverse)
library(suncalc)
library(lubridate)
library(imager)


# Set WD to main folder containing sub-folders for each year & month

# Pull in list of all images
image_files <- list.files(path = "Snow Photos",
                          pattern = "*.jpg",
                          recursive = TRUE,
                          full.names = TRUE)


# Extract EXIF data
exif_dat <- read_exif(image_files, 
                      recursive = FALSE,
                      tags = c("SourceFile", "FileModifyDate"))


# Separate Date & Time columns
file_dat <- exif_dat %>% 
  separate(col = "FileModifyDate",           # Split col into Date & Time cols
           into = c("Date", "Time"),
           sep = " ") %>% 
  separate(col = "Date",                     # Split Date col into M, D, Y cols
           into = c("Year", "Month", "Day"),
           sep = ":") %>% 
  separate(col = "Time",                     # Split time col
           into = c("Time", NA),
           sep = "-") %>% 
  mutate(Date = mdy(paste(Month, Day, Year, sep="/"))) # Created Date col w/ correct formatting



# Create a vector of all unique dates
date_vec <- file_dat %>% 
  group_by(Date) %>%                 # Likely a better way to do this...
  summarise(Time = first(Time)) %>%  # This summarise is just a placeholder for getting the
  ungroup() %>%                      # vector of unique dates
  pull(Date)


# Get sunrise times for all dates
sunrise_times <- getSunlightTimes(date = date_vec, lat = 44.0639, lon = -71.2874, 
                                  keep = c("sunrise"),
                                  tz = "America/New_York") %>% 
                 select(date, sunrise)


# Join sunrise times back to main exif data frame
joined_df <- left_join(file_dat, sunrise_times, by = c("Date" = "date"))


final_df <- joined_df %>% 
  # Create datetime column for when photo was taken
  mutate(PhotoTime = as.POSIXct(paste(Date, Time), 
                                format="%Y-%m-%d %H:%M:%S")) %>% 
  
  # Calculate time difference between when photo was taken and sunrise time of that day
  mutate(TimeAfterSunrise = as.numeric(difftime(PhotoTime, sunrise, units="hours"))) %>% 
  
  # Filter for photos taken >= 1 hour after sunrise
  filter(TimeAfterSunrise >= 1) %>% 
  
  # Select only columns needed for final spreadsheet
  select(SourceFile, Date, Year, Month, Day, Time) %>% 
  
  # Group by date to get only 1 photo per day
  group_by(Date) %>% 
  summarise(SourceFile = first(SourceFile),
            Time = first(Time)) %>%             # This should give us just the first photo of the day
  
  # Break Date column back up into individual components for easier sorting
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% 
  
  # Reselect final columns wanted in order
  select(SourceFile, Year, Month, Day, Time)


# Save output as a csv file
# write.csv(file = "BEF Snow 2020 Subsetted.csv", final_df, row.names = FALSE)



## Subsetting the photos

# Create vector of subsetted file names with full path
subset_files <- final_df %>% pull(SourceFile)


# For loop for saving subsetted photos
# For now have to manually create folders for each year/month in order for it to sort them
for(index in subset_files){
  
  # Define the file path for original photos to sort through  
  fpath <- paste0("C:/Users/jswil/Documents/R/PROJECTS/PhD/BEF Snow Depth Photos/", index)
  
  
  # Read each file from the subset vector
  temp <- load.image(fpath) 
  
  
  # Use str_remove to remove the old file path and .jpg extension from the index
  # Use new, simplified file name to create a save path to a new folder
  save_path <- paste0("C:/Users/jswil/Documents/R/PROJECTS/PhD/BEF Snow Depth Photos/Subsetted Photos/",
                      str_remove(index, 
                                 pattern = ".jpg") %>% # Remove .jpg extension
                        str_remove(pattern = "Snow Photos/"), # Remove old file path
                      ".jpg")
  
  
  # Save as new file in new folder
  save.image(temp, file = save_path)
  
}
