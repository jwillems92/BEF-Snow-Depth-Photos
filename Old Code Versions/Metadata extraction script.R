
# Extracting Metadata from BEF Snow Depth Images

library(exifr)
library(tidyverse)

# Set WD to main folder containing sub-folders for each year & month


# Pull in list of all images
image_files <- list.files(path = "Snow Photos",
                          pattern = "*.jpg",
                          recursive = TRUE,
                          full.names = TRUE)

# Extract EXIF data
exif_dat <- read_exif(image_files, 
                      tags = c("FileName", "FileModifyDate"),
                      recursive = FALSE)

# Separate Date & Time columns, save file of metadata as csv
exif_dat %>% 
  select(-c(SourceFile)) %>% 
  separate(col = "FileModifyDate",           # Split col into Date & Time cols
           into = c("Date", "Time"),
           sep = " ") %>% 
  separate(col = "Date",                     # Split Date col into M, D, Y cols
           into = c("Year", "Month", "Day"),
           sep = ":") %>% 
  separate(col = "Time",                     # Split time col
           into = c("Time", NA),
           sep = "-") %>% 
  write.csv(file = "BEF_SnowDepth_2020.csv", row.names = FALSE) # Write file
