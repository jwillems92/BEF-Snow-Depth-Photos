

### Script for sub-setting NEON snow depth photos from BEF ####

library(tidyverse)
library(suncalc)
library(lubridate)
library(imager)


# Pull in list of all images
image_files <- list.files(path = "Snow Photos",
                          pattern = "*.jpg",
                          recursive = TRUE,
                          full.names = TRUE)


# Convert vector of image files to dataframe for manipulation
image_files <- as.data.frame(image_files) %>% 
                  rename("SourceFile" = "image_files")



# Formatting dates and times
file_dat <- image_files %>% 
  
  #Duplicate column for extracting dates and times
  mutate(DateTime = SourceFile) %>% 
  
  # Remove file path and site ID portion of file names
  mutate(DateTime = str_remove(DateTime, 
                               pattern = "Snow Photos/\\d+/\\d+/NEON.D01.BART.DP1.00042_")) %>% 
  
  # Split the DateTime column out into individual components
  separate(DateTime,
           into = c("Year", "Month", "Day", "Time"),
           sep = "_") %>% 
  
  # Remove the .jpg extension from each file name
  mutate(Time = str_remove(Time,
                           pattern = ".jpg")) %>% 
  
  # Format the time column correctly
  mutate(Time = paste0(substr(Time,1,2),":",substr(Time,3,4),":", substr(Time,5,6))) %>% 
  
  # Create a correctly formatted Date column
  mutate(Date = mdy(paste(Month, Day, Year, sep = "/"))) %>% 
  
  # Create a Datetime column for time photo was taken
  mutate(PhotoTime = as.POSIXct(paste(Date, Time), 
                                format="%Y-%m-%d %H:%M:%S")) %>% 
  
  # Select only column of interest
  select(SourceFile, Year, Month, Day, PhotoTime, Date)


# Create a vector of all unique dates
date_vec <- file_dat %>% 
              pull(Date) %>% 
              unique()


# Get sunrise times for all dates & join back to exif data frame
sunrise_times <- getSunlightTimes(date = date_vec, lat = 44.0639, lon = -71.2874, 
                                  keep = c("sunrise"),
                                  tz = "America/New_York") %>% 
                 select(date, sunrise)

joined_df <- left_join(file_dat, sunrise_times, by = c("Date" = "date"))



final_df <- joined_df %>% 
  
  # Calculate time difference between when photo was taken and sunrise time of that day
  mutate(TimeAfterSunrise = as.numeric(difftime(PhotoTime, sunrise, units="hours"))) %>% 
  
  # Filter for photos taken >= 1 hour after sunrise
  filter(TimeAfterSunrise >= 1) %>% 
  
  # Select only columns needed for final spreadsheet
  select(SourceFile, Date, Year, Month, Day, PhotoTime) %>% 
  
  # Group by date to get only 1 photo per day
  group_by(Date) %>% 
  summarise(SourceFile = first(SourceFile),
            PhotoTime = first(PhotoTime)) %>%     # Get just the first photo of each day
  
  # Break Date column back up into individual components for easier sorting
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% 
  
  # Reselect final columns wanted in order
  select(SourceFile, Year, Month, Day, PhotoTime) %>% 
  
  # Remove date from PhotoTime column
  separate(PhotoTime,
           into = c(NA, "Time"),
           sep = " ")


# Clean up output and save as a csv file
#final_df %>% 
#  mutate(SourceFile = str_remove(SourceFile, pattern = "Snow Photos/\\d+/\\d+/")) %>% 
#  write.csv(file = "BEF Snow 2020 Subsetted.csv", row.names = FALSE)



## Subsetting the photos

# Create vector of subsetted file names with full path
subset_files <- final_df %>% pull(SourceFile)


# For loop for saving subsetted photos
for(index in subset_files){
  
  # Define the file path for original photos to sort through  
  fpath <- paste0("C:/Users/jswil/Documents/R/PROJECTS/PhD/BEF Snow Depth Photos/", index)
  
  
  # Create new folders to save subsetted photos into (throws warnings but seems to run fine)
  # Warning due to R trying to create new folder for every file
  # Could try a nested for loop to create folders but this works ok for now
  cwd <- getwd()
  mkfldr <- paste0("Subsetted Photos/",
                   str_remove(index, 
                              pattern = "Snow Photos/") %>% 
                     str_remove(pattern = "/NEON.D01.BART.DP1.00042_\\d+_\\d+_\\d+_\\d+.jpg"))
  dir.create(file.path(cwd, mkfldr), recursive = TRUE)
  
  
  # Read each file from the subset vector
  temp <- load.image(fpath) 
  
  
  # Use str_remove to remove the old file path and .jpg extension from the index
  # Use new, simplified file name to create a save path to a new folder
  save_path <- paste0("C:/Users/jswil/Documents/R/PROJECTS/PhD/BEF Snow Depth Photos/Subsetted Photos/",
                      str_remove(index, 
                                 pattern = ".jpg") %>% # Remove .jpg extension
                        str_remove(pattern = "Snow Photos/"), # Remove old file path
                      ".jpg") # Add .jpg extension back in to save file
  
  
  # Save as new file in new folder
  save.image(temp, file = save_path)
  
}